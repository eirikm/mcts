package mcts
package games

import mcts.data.Array2d
import mcts.data.Array2d.{Column,                  Row}
import mcts.games.ConnectFourWinning.{VictoryLine, VictoryLinesForPoint}

object ConnectFour {
  val NumRows:  Int = 6
  val NumCols:  Int = 7
  val FirstRow: Row = Row(0)
  val LastRow:  Row = Row(NumRows - 1)

  val EmptyState = State(
    board         = Array2d(numX = NumCols, numY = NumRows, empty = Empty),
    currentPlayer = Red,
    lastActionOpt = None,
    numRounds     = 0
  )

  case class LastAction(player: RedBlue, column: Column, row: Row)

  case class State(
      board:         Array2d[Space],
      currentPlayer: RedBlue,
      lastActionOpt: Option[LastAction],
      numRounds:     Int
  ) {

    lazy val winnerOpt: Option[(RedBlue, VictoryLine)] =
      if (numRounds < 7) None
      else lastActionOpt.flatMap(lastAction => wasWinningMove(board, lastAction))

    lazy val result: GameResult[Column, RedBlue] =
      winnerOpt match {
        case Some((winner, _)) => Winner(winner)
        case None =>
          availableColumns(board) match {
            case cols if cols.isEmpty => Draw
            case cols                 => Ongoing(cols)
          }
      }
  }

//  def availableColumns(board: Array2d[Space]): Array[Column] =
//    Cols.filter((col: Column) => board(col, FirstRow) == Empty)

  def availableColumns(board: Array2d[Space]): Array[Column] = {
    /* Trade some allocations for cpu and count first. */
    var col   = 0
    var count = 0
    while (col < NumCols) {
      if (board(col, FirstRow) == Empty) {
        count += 1
      }
      col += 1
    }

    val ret = Array.ofDim[Column](count)
    col = 0
    var marked = 0
    while (col < NumCols) {
      if (board(col, FirstRow) == Empty) {
        ret(marked) = col
        marked += 1
      }
      col += 1
    }
    ret
  }

  /* Get the row that the piece settles in if we drop it at that column. */
  def availableRow(board: Array2d[Space], col: Column): Row = {
    var row = 0
    while (row < NumRows) {
      if (board(col, row) != Empty) {
        return Row(row - 1)
      }
      row += 1
    }
    LastRow
  }

  object GameDef extends Game[State, Column, RedBlue] {
    override def gameResult(state: State): GameResult[Column, RedBlue] =
      state.result

    override def currentPlayer(state: State): RedBlue =
      state.currentPlayer

    override val startingState: State =
      EmptyState

    override def nextState(col: Column, state: State): State = {
      val row     = availableRow(state.board, col)
      val player  = state.currentPlayer
      val newGrid = state.board.updated(col, row, Occupied(player))

      State(
        board         = newGrid,
        currentPlayer = player.other,
        lastActionOpt = Some(LastAction(player, col, row)),
        numRounds     = state.numRounds + 1
      )
    }
  }

  /* An O(1) win check for connect 4 around a piece that is independent of board size. */
  def wasWinningMove(board: Array2d[Space], lastAction: LastAction): Option[(RedBlue, VictoryLine)] = {
    val victoryLines: Array[VictoryLine] =
      VictoryLinesForPoint(lastAction.column, lastAction.row)

    var idx = 0
    while (idx < victoryLines.length) {
      val victoryLine = victoryLines(idx)
      val winOpt      = victoryLine.matches(board, lastAction.player)

      if (winOpt.isDefined) return winOpt

      idx += 1
    }
    None
  }
}
