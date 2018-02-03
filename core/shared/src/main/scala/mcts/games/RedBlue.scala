package mcts.games

sealed trait RedBlue {
  def other: RedBlue =
    this match {
      case Red  => Blue
      case Blue => Red
    }
}
case object Red  extends RedBlue
case object Blue extends RedBlue
