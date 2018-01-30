package haskell

import java.lang.System.currentTimeMillis

import scala.annotation.tailrec
import scala.collection.SortedMap
import scala.concurrent.duration.Duration

object MCTS {

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
        meanPayouts = StdLib.unionWith(payouts, meanPayouts, addToMean),
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
