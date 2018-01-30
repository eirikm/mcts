import java.lang.System.currentTimeMillis

import scala.annotation.tailrec
import scala.collection.SortedMap
import scala.concurrent.duration.Duration

object StdLib {
  def unionWith[K: Ordering, V](m1: SortedMap[K, V], m2: SortedMap[K, V], op: (V, V) => V): SortedMap[K, V] = {
    val ret = SortedMap.newBuilder[K, V]

    (m1.keys ++ m2.keys).toSet.foreach { key: K =>
      (m1.get(key), m2.get(key)) match {
        case (Some(v1), Some(v2)) => ret += ((key, op(v1, v2)))
        case (Some(v1), None)     => ret += ((key, v1))
        case (None, Some(v2))     => ret += ((key, v2))
        case (None, None)         =>
      }
    }
    ret.result()
  }

  trait StdGen {
    def nextInt: (Int, StdGen)

    final def nextInt(bound: Int): (Int, StdGen) = {
      val (i, next) = nextInt
      (i % bound, next)
    }

    final def oneFrom[T](ts: Seq[T]): (T, StdGen) = {
      val (idx, next) = nextInt(ts.size)
      (ts(idx), next)
    }
  }

  object StdGen {
    def simple(seed: Long): StdGen = new StdGen {
      override def nextInt: (Int, StdGen) = {
        val seed2 = (seed * 0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
        val ret   = math.abs((seed2 >>> 16).toInt)
        (ret, simple(seed2))
      }
    }
  }
}

object MCTS {

  import StdLib._

  // A game specification, containing the state type, action type and player type.
  trait Spec[S, A, P] {
    // Returns the legal actions the player can perform from this state,
    // preceded by their weights, representing how likely the player will play
    // the state.
    //
    // A final state returns empty.
    def actions(state: S): Seq[(Double, A)]

    // The player whose utilities are maximized for this state.
    def player(state: S): P

    // If the state is a final state, returns a "map" containing pairs of
    // players and their scores. Each player is assumed to maximize their
    // payouts.
    def payouts(state: S): SortedMap[P, Double]

    // Apply an action to a state, returning the resulting state.
    def apply(action: A, state: S): S

    def winner(state: S): Option[P]
  }

  // A node in the monte carlo search tree.
  case class Node[A, P: Ordering](
      // The children of the node. Each edge is an action that results in the child node.
      children: SortedMap[A, Node[A, P]],
      // The mean payouts for each player in the game.
      meanPayouts: SortedMap[P, Double],
      // The number of times this node was "played".
      playCount: Double
  ) {
    // Update a node with the payout.
    def withPayouts(payouts: SortedMap[P, Double]): Node[A, P] = {
      // Add a number to a mean, given we know how many numbers make up the mean.
      def addToMean(number: Double, mean: Double): Double =
        (mean * playCount + number) / (playCount + 1)

      copy(
        meanPayouts = unionWith(payouts, meanPayouts, addToMean),
        playCount = playCount + 1
      )
    }

    // Update a node with the payout and the updated child node.
    def backprop(action: A, payouts: SortedMap[P, Double], child: Node[A, P]): Node[A, P] =
      withPayouts(payouts).copy(children = children + (action -> child))
  }

  object Node {
    def empty[A: Ordering, P: Ordering]: Node[A, P] =
      Node[A, P](SortedMap.empty, SortedMap.empty, 0.0)

    def singleton[A: Ordering, P: Ordering](payoutMap: SortedMap[P, Double]): Node[A, P] =
      Node[A, P](SortedMap.empty, payoutMap, 1.0)
  }

  //
  // Monte Carlo Tree Search
  //
  class MonteCarlo[S, A: Ordering, P: Ordering](spec: Spec[S, A, P]) {
    // Run monte carlo search until cpuTime hits a certain value. Prevents issues
    // clock drift during computation.
    def timedMCTS(duration: Duration, r: StdGen, state: S, n: Node[A, P]): (StdGen, Node[A, P]) = {
      val stopTime: Long = currentTimeMillis + duration.toMillis

      @tailrec
      def go(r: StdGen, n: Node[A, P]): (StdGen, Node[A, P]) = {
        val (nr, nn, _) = select(r, state, n)
        if (currentTimeMillis > stopTime) (nr, nn)
        else go(nr, nn)
      }

      val tuple = go(r, n)
      tuple
    }

    // Returns the best action from a given node and state.
    def bestAction(node: Node[A, P], state: S): A =
      node.children.mapValues(child => child.meanPayouts(spec.player(state))).maxBy(_._2)._1

    // Run monte carlo simulation, returning the updated node, the updated
    // random generator, and the resulting payout of the simulation.
    def select(rand: StdGen, state: S, node: Node[A, P]): (StdGen, Node[A, P], SortedMap[P, Double]) =
      spec.actions(state) match {
        case Nil =>
          // Update the node's payouts assuming the state is the final state.
          val payOuts = spec.payouts(state)
          (rand, node.withPayouts(payOuts), payOuts)
        case actions =>
          // Looks for an unexpanded action to start simulation from.
          val unexpandedActionOpt: Option[A] =
            actions.collectFirst {
              case (_, a) if !node.children.contains(a) => a
            }

          unexpandedActionOpt match {
            // Recursively call select on a child of this tree based on UCT.
            case None =>
              val action                       = uct(state, node)
              val (newRand, child, newPayouts) = select(rand, spec.apply(action, state), node.children(action))
              val newNode                      = node.backprop(action, newPayouts, child)
              (newRand, newNode, newPayouts)

            // Perform expansion by creating a new node and simulate from there
            case Some(unexpandedAction) =>
              val newState              = spec.apply(unexpandedAction, state)
              val (newRand, newPayouts) = simulate(rand, newState)
              val child                 = Node.singleton[A, P](newPayouts)
              val newNode               = node.backprop(unexpandedAction, newPayouts, child)
              (newRand, newNode, newPayouts)
          }
      }

    // Simulates the game randomly from a starting state.
    def simulate(rand: StdGen, state: S): (StdGen, SortedMap[P, Double]) =
      spec.actions(state) match {
        case Nil => (rand, spec.payouts(state))
        case actions =>
          val (childAction, newRand) = rand.oneFrom(actions) //todo: consider distribution or remove
          val childState             = spec.apply(childAction._2, state)

          simulate(newRand, childState)
      }

    // Finds the best action under UCB1 to continue selection.
    // This function assumes that there are no unexpanded nodes or terminal nodes.
    def uct(state: S, node: Node[A, P]): A = {
      def ucb(child: Node[A, P]): Double =
        child.meanPayouts(spec.player(state)) + math.sqrt(2 * math.sqrt(math.log(node.playCount) / child.playCount))

      spec.actions(state).maxBy(a => ucb(node.children(a._2)))._2
    }
  }
}

object TicTacToe {
  type Draw = (Int, Int)

  sealed trait Player
  case object Max extends Player
  case object Min extends Player

  sealed trait Space
  case object Empty extends Space
  case class Occupied(player: Player) extends Space

  case class State(grid: SortedMap[Draw, Space], turn: Player)

  implicit val OrderingPlayer: Ordering[Player] =
    Ordering.by[Player, String](_.toString)

  implicit val OrderingSpace: Ordering[Space] =
    Ordering.by[Space, Option[Player]] {
      case Occupied(p) => Option(p)
      case Empty       => None
    }

  val EmptyState: State = {
    val grid = for {
      x <- 0 until 3
      y <- 0 until 3
    } yield (x, y) -> (Empty: Space)
    State(SortedMap.empty[Draw, Space] ++ grid, Max)
  }

  lazy val checkMatrix: Seq[Seq[Draw]] =
    Seq(
      Seq((0, 0), (0, 1), (0, 2)),
      Seq((1, 0), (1, 1), (1, 2)),
      Seq((2, 0), (2, 1), (2, 2)),
      Seq((0, 0), (1, 0), (2, 0)),
      Seq((0, 1), (1, 1), (2, 1)),
      Seq((0, 2), (1, 2), (2, 2)),
      Seq((0, 0), (1, 1), (2, 2)),
      Seq((0, 2), (1, 1), (2, 0))
    )

  def isLine(arr: SortedMap[Draw, Space])(check: Seq[Draw]): Option[Player] =
    if (check.forall(c => arr(c) == Occupied(Min))) Some(Min)
    else if (check.forall(c => arr(c) == Occupied(Max))) Some(Max)
    else None

  object Game extends MCTS.Spec[State, Draw, Player] {
    // Returns positions where the columns are not filled up.
    override def actions(s: State): Seq[(Double, (Int, Int))] =
      winner(s) match {
        case Some(_) => IndexedSeq.empty
        case None =>
          s.grid.toSeq.collect {
            case (pos, Empty) => 1.0 -> pos
          }
      }

    override def player(state: State): Player =
      state.turn

    // Returns a payout of 1 if we won, 0 if we lost.
    override def payouts(s: State): SortedMap[Player, Double] =
      winner(s) match {
        case Some(Max) => SortedMap[Player, Double](Max -> 1.0, Min  -> -1.0)
        case Some(Min) => SortedMap[Player, Double](Max -> -1.0, Min -> 1.0)
        case None      => SortedMap[Player, Double](Max -> 0.0, Min  -> 0.0)
      }

    // Applies action a to board b.
    override def apply(action: Draw, state: State): State =
      State(
        state.grid.updated(action, Occupied(state.turn)),
        if (state.turn == Max) Min else Max
      )

    override def winner(state: State): Option[Player] =
      checkMatrix.flatMap(c => isLine(state.grid)(c)).headOption
  }
}

object HaskMain extends App {
  import scala.concurrent.duration._

  @tailrec
  def continue[S, A: Ordering, P: Ordering](spec: MCTS.Spec[S, A, P])(rand: StdLib.StdGen, state: S): StdLib.StdGen = {

    val mcts                 = new MCTS.MonteCarlo(spec)
    val (newRand, finalNode) = mcts.timedMCTS(10.millis, rand, state, MCTS.Node.empty[A, P])

    // Check for an inevitable tie.
    if (spec.actions(state).isEmpty) {
      println("It's a Tie!")
      newRand
    } else {
      val computerAction = mcts.bestAction(finalNode, state)
      println(s"${spec.player(state)} Chose: $computerAction")

      val nextState = spec.apply(computerAction, state)
      println(nextState)
      spec.winner(nextState) match {
        case Some(winner) =>
          println(s"$winner won")
          newRand
        case None => continue(spec)(newRand, nextState)
      }
    }
  }

  continue(TicTacToe.Game)(StdLib.StdGen.simple(currentTimeMillis()), TicTacToe.EmptyState)
}
