package mcts.games

import mcts._
import mcts.data.Array2d
import mcts.data.Array2d.Index

import scala.collection.mutable

object TicTacToe {

  object GameDef extends Game[State, Index, RedBlue] {
    override def gameResult(state: State): GameResult[Index, RedBlue] =
      state.result

    override def currentPlayer(s: State): RedBlue =
      s.currentPlayer

    override def nextState(a: Index, s: State): State =
      State(
        grid          = s.grid.updated(a, Occupied(s.currentPlayer)),
        currentPlayer = s.currentPlayer.other
      )

    override val startingState: State =
      State(
        grid          = Array2d(3, 3, Empty),
        currentPlayer = Red
      )
  }

  case class State(grid: Array2d[Space], currentPlayer: RedBlue) {
    lazy val winnerOpt: Option[(RedBlue, VictoryLine)] =
      winner(grid)

    lazy val result: GameResult[Index, RedBlue] =
      winnerOpt match {
        case Some((p, _)) => Winner(p)
        case None =>
          possibleActions(grid) match {
            case actions if actions.isEmpty => Draw
            case actions                    => Ongoing(actions)
          }
      }
  }

  def possibleActions(grid: Array2d[Space]): Array[Index] = {
    val builder = mutable.ArrayBuilder.make[Index]
    var idx     = 0
    while (idx < grid.array.length) {
      if (grid.array(idx) == Empty) {
        builder += Index(idx)
      }
      idx += 1
    }
    builder.result()
  }

  /**
    * An absolute, existing line which can form a victory for one of the players
    */
  final class VictoryLine(_1: Index, _2: Index, _3: Index) {
    @inline
    def matches(board: Array2d[Space]): Option[(RedBlue, VictoryLine)] =
      (board(_1), board(_2), board(_3)) match {
        case (Occupied(p1), Occupied(p2), Occupied(p3)) if p1 == p2 && p2 == p3 =>
          Some((p1, this))
        case _ =>
          None
      }

    def contains(idx: Index): Boolean =
      _1 == idx || _2 == idx || _3 == idx
  }

  val VictoryLines: Array[VictoryLine] =
    Array(
      new VictoryLine(Index(0), Index(3), Index(6)),
      new VictoryLine(Index(1), Index(4), Index(7)),
      new VictoryLine(Index(2), Index(5), Index(8)),
      new VictoryLine(Index(0), Index(1), Index(2)),
      new VictoryLine(Index(3), Index(4), Index(5)),
      new VictoryLine(Index(6), Index(7), Index(8)),
      new VictoryLine(Index(0), Index(4), Index(8)),
      new VictoryLine(Index(6), Index(4), Index(2))
    )

  def winner(board: Array2d[Space]): Option[(RedBlue, VictoryLine)] = {
    var idx = 0
    while (idx < VictoryLines.length) {
      val victoryLine = VictoryLines(idx)
      val winOpt      = victoryLine.matches(board)

      if (winOpt.isDefined) return winOpt

      idx += 1
    }
    None
  }
}
