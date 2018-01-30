package haskell

import scala.collection.SortedMap

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
