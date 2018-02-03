package mcts.games

sealed trait Space
case object Empty                   extends Space
case class Occupied(player: RedBlue) extends Space
