package mcts.games

import mcts.data.Array2d
import mcts.data.Array2d.{Column, Index, Row}
import mcts.games.ConnectFour.EmptyState

object ConnectFourWinning {
  /* A point relative to another point */
  private case class RelPoint(x: Int, y: Int)

  /* A line relative to a point */
  private case class RelLine(_1: RelPoint, _2: RelPoint, _3: RelPoint, _4: RelPoint)

  /* These describe all winning lines a given point can theoretically be a part of.
   * Many of them will be out of bounds for a given point, so those lines are pruned below
   */
  private val relativeVictoryLines: Array[RelLine] =
    Array(
      // format: off
      /* horizontal */
      RelLine(RelPoint(x =  0, y =  0), RelPoint(x =  1, y =  0), RelPoint(x =  2, y =  0), RelPoint(x = 3, y = 0)),
      RelLine(RelPoint(x = -1, y =  0), RelPoint(x =  0, y =  0), RelPoint(x =  1, y =  0), RelPoint(x = 2, y = 0)),
      RelLine(RelPoint(x = -2, y =  0), RelPoint(x = -1, y =  0), RelPoint(x =  0, y =  0), RelPoint(x = 1, y = 0)),
      RelLine(RelPoint(x = -3, y =  0), RelPoint(x = -2, y =  0), RelPoint(x = -1, y =  0), RelPoint(x = 0, y = 0)),

      /* vertical */
      RelLine(RelPoint(x =  0, y =  0), RelPoint(x =  0, y =  1), RelPoint(x =  0, y =  2), RelPoint(x = 0, y = 3)),
      RelLine(RelPoint(x =  0, y = -1), RelPoint(x =  0, y =  0), RelPoint(x =  0, y =  1), RelPoint(x = 0, y = 2)),
      RelLine(RelPoint(x =  0, y = -2), RelPoint(x =  0, y = -1), RelPoint(x =  0, y =  0), RelPoint(x = 0, y = 1)),
      RelLine(RelPoint(x =  0, y = -3), RelPoint(x =  0, y = -2), RelPoint(x =  0, y = -1), RelPoint(x = 0, y = 0)),

      /* diagonal */
      RelLine(RelPoint(x =  0, y =  0), RelPoint(x =  1, y =  1), RelPoint(x =  2, y =  2), RelPoint(x = 3, y = 3)),
      RelLine(RelPoint(x = -1, y = -1), RelPoint(x =  0, y =  0), RelPoint(x =  1, y =  1), RelPoint(x = 2, y = 2)),
      RelLine(RelPoint(x = -2, y = -2), RelPoint(x = -1, y = -1), RelPoint(x =  0, y =  0), RelPoint(x = 1, y = 1)),
      RelLine(RelPoint(x = -3, y = -3), RelPoint(x = -2, y = -2), RelPoint(x = -1, y = -1), RelPoint(x = 0, y = 0)),

      RelLine(RelPoint(x =  0, y =  0), RelPoint(x =  1, y = -1), RelPoint(x =  2, y = -2), RelPoint(x = 3, y = -3)),
      RelLine(RelPoint(x = -1, y =  1), RelPoint(x =  0, y =  0), RelPoint(x =  1, y = -1), RelPoint(x = 2, y = -2)),
      RelLine(RelPoint(x = -2, y =  2), RelPoint(x = -1, y =  1), RelPoint(x =  0, y =  0), RelPoint(x = 1, y = -1)),
      RelLine(RelPoint(x = -3, y =  3), RelPoint(x = -2, y =  2), RelPoint(x = -1, y =  1), RelPoint(x = 0, y =  0))
      // format: on
    )

  /**
    * Returns a legal `Index` for `(_col, _row) + relPoint` if it is within bounds
    */
  private def validIndex(relPoint: RelPoint, board: Array2d[Space], _col: Column, _row: Row): Option[Index] = {
    val col = _col + relPoint.x
    val row = _row + relPoint.y

    if (col < board.numX && col >= 0 && row < board.numY && row >= 0)
      Some(board.index(col, row))
    else
      None
  }

  /**
    * Returns a victory line for (col, row) if it is within bounds of the board
    */
  private def validLine(relLine: RelLine, board: Array2d[Space], col: Column, row: Row): Option[VictoryLine] =
    for {
      p1 <- validIndex(relLine._1, board, col, row)
      p2 <- validIndex(relLine._2, board, col, row)
      p3 <- validIndex(relLine._3, board, col, row)
      p4 <- validIndex(relLine._4, board, col, row)
    } yield new VictoryLine(p1, p2, p3, p4)

  /**
    * An absolute, existing line which can form a victory for one of the players
    */
  class VictoryLine(_1: Index, _2: Index, _3: Index, _4: Index) {
    def matches(board: Array2d[Space], player: RedBlue): Option[(RedBlue, VictoryLine)] =
      (board(_1), board(_2), board(_3), board(_4)) match {
        case (Occupied(`player`), Occupied(`player`), Occupied(`player`), Occupied(`player`)) =>
          Some((player, this))
        case _ =>
          None
      }

    def contains(idx: Index): Boolean =
      _1 == idx || _2 == idx || _3 == idx || _4 == idx
  }

  /**
    * Precalculate all victory lines for each point on a board. This is the same for all boards.
    */
  val VictoryLinesForPoint: Array2d[Array[VictoryLine]] =
    EmptyState.board.map(
      (col, row, _) =>
        relativeVictoryLines.flatMap(
          line => validLine(line, EmptyState.board, col, row)
      )
    )
}
