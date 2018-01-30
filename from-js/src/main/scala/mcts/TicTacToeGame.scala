package mcts

import scala.collection.mutable

object TicTacToeGame {
  def apply(): TicTacToeGame =
    new TicTacToeGame(EmptyBoard, PlayerX)

  sealed trait Placement
  sealed trait Player extends Placement
  case object Empty   extends Placement
  case object PlayerX extends Player
  case object PlayerO extends Player

  case class Index(value: Int)

  case class Score(value: Int) extends AnyVal {
    def +(score: Score): Score = Score(value + score.value)
  }

  case class WinningScore(value: Int) extends AnyVal {
    def matches(score: Score): Boolean = (value & score.value) == value
  }

  // See http://jsfiddle.net/5wKfF/249/
  val WinningScores: Seq[WinningScore] =
    Vector(7, 56, 448, 73, 146, 292, 273, 84).map(WinningScore.apply)

  val BoardScore: Map[Index, Score] =
    Seq(1, 2, 4, 8, 16, 32, 64, 128, 256).zipWithIndex.map { case (score, idx) => Index(idx) -> Score(score) }.toMap

  def Board(ps: Placement*): Map[Index, Placement] =
    ps.zipWithIndex.map { case (p, idx) => Index(idx) -> p }.toMap

  val EmptyBoard: Map[Index, Placement] =
    Board(Seq.fill(9)(Empty): _*)

  def otherPlayer(player: Player): Player =
    player match {
      case PlayerX => PlayerO
      case PlayerO => PlayerX
    }
}

import TicTacToeGame._

class TicTacToeGame(val board: Map[Index, Placement], val currentPlayer: Player) extends Game[Player, Index] {
  override type Self = TicTacToeGame

  override def withMove(move: Index): Self =
    new TicTacToeGame(board.updated(move, currentPlayer), otherPlayer(currentPlayer))

  override lazy val possibleMoves: Iterable[Index] =
    board.collect { case (idx, Empty) => idx }

  override lazy val winner: Option[Player] = {
    val playerScores = mutable.Map[Player, Score](PlayerO -> Score(0), PlayerX -> Score(0))

    board.foreach {
      case (idx, player: Player) => playerScores(player) = playerScores(player) + BoardScore(idx)
      case _ =>
    }

    object Winner {
      def unapply(player: Player): Option[Player] =
        WinningScores.collectFirst {
          case winningScore if winningScore.matches(playerScores(player)) => player
        }
    }

    Seq(PlayerO, PlayerX) collectFirst {
      case Winner(player) => player
    }
  }

  override def isRandom: Boolean = false
}
