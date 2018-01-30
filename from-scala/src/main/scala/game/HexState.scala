package game

import bfs.BFS
import bfs.BFS.BFSNode
import mcts.GameState

import scala.collection.mutable

object HexState {
  def apply(nRows: Int,
            nColumns: Int): HexState =
    apply(nRows, nColumns, PlayerTwo, Array.fill(nRows * nColumns)(Empty: BoardNode))

  def apply(nRows: Int,
            nColumns: Int,
            lastPlayerWhoMoved: Player,
            board: Array[BoardNode]): HexState =
    new HexState(Precalc(nRows, nColumns), lastPlayerWhoMoved, board)

  val directions: List[(Int, Int)] =
    List(
      (-1, 0), // up-left
      (-1, +1), // up-right
      (0, -1), // left
      (0, 1), // right
      (+1, -1), // down-left
      (+1, 0) // down-right
    )

  def format(p: BoardNode): String =
    p match {
      case Empty => "."
      case PlayerOne => "1"
      case PlayerTwo => "2"
    }

  object Precalc {
    def apply(nRows: Int, nColumns: Int): Precalc = {
      val nodeMap = mutable.Map.empty[Int, BFSNode]
      val allNodes = mutable.Set.empty[BFSNode]
      val allEdges = mutable.Set.empty[(BFSNode, BFSNode)]

      for (row <- 0 until nRows) {
        for (col <- 0 until nColumns) {
          var node = row * nColumns + col
          allNodes += node
          nodeMap(row * nColumns + col) = node
        }
      }

      for (row <- 0 until nRows) {
        for (col <- 0 until nColumns) {

          //  0  1  2  3        |   (0,0) (0,1) (0,2), (0,3)
          //    4  5  6  7      |      (1,0) (1,1) (1,2) (1,3)
          //      8  9  10 11   |         (2,0) (2,1), (2,2) (2,3)

          for ((dy, dx) <- HexState.directions) {

            val currentNode = nodeMap(row * nColumns + col)

            val neighborCol = col + dx
            val neighborRow = row + dy

            if (neighborRow >= 0 && neighborRow < nRows && neighborCol >= 0 && neighborCol < nColumns) {
              val neighborIndex: Int = neighborRow * nColumns + neighborCol
              val neighborNode: BFSNode = nodeMap(neighborIndex)
              allEdges += ((currentNode, neighborNode))
              allEdges += ((neighborNode, currentNode))
            }
          }

        }
      }
      // player 1 goes horizontal (left edge to right edge)
      val leftIndices = mutable.ListBuffer.empty[Int]
      val rightIndices = mutable.ListBuffer.empty[Int]

      for (row <- 0 until nRows) {
        leftIndices += row * nColumns
        rightIndices += (row + 1) * nColumns - 1
      }

      // player 2 goes vertical (top edge to bottom edge)
      val topIndices = mutable.ListBuffer.empty[Int]
      val bottomIndices = mutable.ListBuffer.empty[Int]
      for (col <- 0 until nColumns) {
        topIndices += col
        bottomIndices += (nRows - 1) * nColumns + col
      }

      new Precalc(
        nRows,
        nColumns,
        nodeMap.toMap,
        allNodes.to[Set],
        allEdges.to[Set],
        leftIndices.toArray,
        rightIndices.toArray,
        topIndices.toArray,
        bottomIndices.toArray
      )
    }
  }

  private[HexState] class Precalc(val nRows: Int,
                                  val nColumns: Int,
                                  val nodeMap: Map[Int, BFSNode],
                                  val allNodes: Set[BFSNode],
                                  val allEdges: Set[(BFSNode, BFSNode)],
                                  val leftIndices: Array[Int],
                                  val rightIndices: Array[Int],
                                  val topIndices: Array[Int],
                                  val bottomIndices: Array[Int])

}

/**
  * Created by culim on 2/24/16.
  */
class HexState(precalc: HexState.Precalc,
               val lastPlayerWhoMoved: Player,
               val board: Array[BoardNode]) extends GameState {
  override type Self = HexState

  override def getAvailableActions: Set[Int] = {
    getPlayerInWinConditions match {
      // Someone has already won, no more actions permitted.
      case (_: Player, _) => Set.empty
      case (Empty, _) =>
        board.zipWithIndex
          .collect { case (Empty, idx) => idx }
          .to[Set]
    }
  }

  override def doAction(action: Int): Self = {
    val newBoard = board.clone()
    val newPlayer = lastPlayerWhoMoved.other
    newBoard(action) = newPlayer
    new HexState(precalc, newPlayer, newBoard)
  }

  override def getLastPlayerWhoMoved: Player =
    lastPlayerWhoMoved

  def getResult: Result =
    getPlayerInWinConditions match {
      case (Empty, _) => Draw
      case (player: Player, _) => Winner(player)
    }

  def getPlayerInWinConditions: (BoardNode, Array[BFSNode]) = {

    val leftIndices = precalc.leftIndices.filter(index => board(index) == PlayerOne)
    val rightIndices = precalc.rightIndices.filter(index => board(index) == PlayerOne)

    if (leftIndices.nonEmpty && rightIndices.nonEmpty) {
      // possibility of win condition for player 1
      val filteredNodes = precalc.allNodes.filter(node => board(node) == PlayerOne)
      val filteredEdges = precalc.allEdges.filter(pair => filteredNodes.contains(pair._1) && filteredNodes.contains(pair._2))

      for (startIndex <- leftIndices) {
        for (endIndex <- rightIndices) {
          //                    println(s"Checking P1 for start=$startIndex end=$endIndex")
          val path: Array[BFSNode] =
            BFS.search(
              filteredNodes,
              filteredEdges,
              precalc.nodeMap(startIndex),
              precalc.nodeMap(endIndex)
            )

          if (path.nonEmpty) {
            return (PlayerOne, path) // player 1
          }
        }
      }
    }

    val topIndices = precalc.topIndices.filter(index => board(index) == PlayerTwo)
    val bottomIndices = precalc.bottomIndices.filter(index => board(index) == PlayerTwo)

    if (topIndices.nonEmpty && bottomIndices.nonEmpty) {
      // possibility of win condition for player 1
      val filteredNodes = precalc.allNodes.filter(node => board(node) == PlayerTwo) // player 2
      val filteredEdges = precalc.allEdges.filter(pair => filteredNodes.contains(pair._1) && filteredNodes.contains(pair._2))

      for (startIndex <- topIndices) {
        for (endIndex <- bottomIndices) {
          //                    println(s"Checking P2 for start=$startIndex end=$endIndex")

          val path: Array[BFSNode] =
            BFS.search(
              filteredNodes,
              filteredEdges,
              precalc.nodeMap(startIndex),
              precalc.nodeMap(endIndex)
            )

          if (path.nonEmpty) {
            return (PlayerTwo, path) // player 2
          }
        }
      }
    }
    (Empty, Array.empty)
  }

  override def toString: String = {
    var s = ""

    for (row <- 0 to precalc.nRows.toString.length) {
      s += " "
    }

    val (winner, path) = getPlayerInWinConditions

    // -- print the headers (top)
    for (col <- 0 until precalc.nColumns) {
      s += ("a".charAt(0) + col).toChar
      if (col < precalc.nColumns - 1) {
        s += " "
      }
    }
    s += "\n"

    // -- print the board
    for (row <- 0 until precalc.nRows) {

      if (row < 10 && precalc.nRows >= 10) {
        s += "0"
      }

      s += row.toString
      s += "  "

      for (buffer <- 1 to row) {
        s += " "
      }
      for (col <- 0 until precalc.nColumns) {
        if (path.nonEmpty && board(row * precalc.nColumns + col) == winner && path.contains(row * precalc.nColumns + col)) {
          s += "*"
        }
        else {
          s += HexState.format(board(row * precalc.nColumns + col))
        }
        if (col < precalc.nColumns - 1) {
          s += " "
        }
      }
      s += "\n"
    }
    s
  }
}
