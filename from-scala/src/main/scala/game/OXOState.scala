package game

import mcts.GameState

object OXOState {
  def format(p: BoardNode): String =
    p match {
      case Empty => "."
      case PlayerOne => "O"
      case PlayerTwo => "X"
    }

  val winConfigurations: Set[(Int, Int, Int)] =
    Set(
      (0, 1, 2), (3, 4, 5), (6, 7, 8), // horizontals
      (0, 3, 6), (1, 4, 7), (2, 5, 8), // verticals
      (0, 4, 8), (2, 4, 6) // diagonals
    )
}

/**
  * Created by culim on 2/24/16.
  */
case class OXOState(
                lastPlayerWhoMoved: Player = PlayerTwo,
                board: Array[BoardNode] = Array.fill(9)(Empty: BoardNode)
              ) extends GameState {

  override type Self = OXOState

  override lazy val toString: String = {
    var s = ""
    for ((value, index) <- board.zipWithIndex) {
      s += OXOState.format(value)
      if (index % 3 == 2) {
        s += "\n"
      }
    }
    s
  }

  override def getLastPlayerWhoMoved: Player =
    lastPlayerWhoMoved

  override lazy val getAvailableActions: Set[Int] = {
    getPlayerInWinConditions match {
      case None =>
        board.zipWithIndex
          .collect { case (Empty, idx) => idx }
          .to[Set]

      // Someone has already won, no more actions permitted.
      case Some(_) => Set.empty
    }
  }

  /**
    * Perform an action on the board by choosing a grid index
    * to place a mark.
    *
    * @param action the grid index that the player wants to choose
    */
  override def doAction(action: Int): Self = {
    val newBoard = board.clone()
    val newPlayer = lastPlayerWhoMoved.other
    newBoard(action) = newPlayer
    copy(
      lastPlayerWhoMoved = newPlayer,
      board = newBoard
    )
  }

  override def getResult: Result =
    getPlayerInWinConditions match {
      case None => Draw
      case Some(player) => Winner(player)
    }

  def getPlayerInWinConditions: Option[Player] =
    OXOState.winConfigurations
      .filter((x: (Int, Int, Int)) => board(x._1) == board(x._2) && board(x._2) == board(x._3))
      .map(x => board(x._1)).collectFirst{
      case p: Player => p
    }
}
