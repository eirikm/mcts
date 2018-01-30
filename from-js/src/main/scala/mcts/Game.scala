package mcts

trait Game[Player, Move] {
  val board: Any
  type Self <: Game[Player, Move]
  def withMove(move: Move): Self
  def isRandom:      Boolean
  def possibleMoves: Iterable[Move]
  def winner:        Option[Player]
  val currentPlayer: Player
}
