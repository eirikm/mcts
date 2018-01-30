package mcts

import org.scalatest.{FunSuite, Matchers}

class TicTacToeTest extends FunSuite with Matchers {
  import TicTacToeGame._

  test("should favor the winning move in a game of Tic Tac Toe") {
    val board: Map[Index, Placement] =
      TicTacToeGame.Board(
        PlayerO, PlayerO, Empty,
        PlayerX, PlayerX, Empty,
        PlayerO, PlayerO, Empty
      )

    val mcts = new MCTS(new TicTacToeGame(board, PlayerX), rounds = 1000, new util.Random(0L))
    mcts.selectMove should equal (Some(Index(5)))
  }

  test("should block the winning move in a game of Tic Tac Toe") {
    val board: Map[Index, Placement] =
      /* Had to change two positions here to make the test run.
        I'm not sure if it made sense before or not*/

      TicTacToeGame.Board(
        /* was Empty! */ PlayerX, Empty, PlayerO,
        /* was Empty!  */ PlayerX, PlayerO, Empty,
        Empty, Empty, Empty
      )

    val mcts = new MCTS(new TicTacToeGame(board, PlayerX), rounds = 1000, new util.Random(0L))
    mcts.selectMove should equal (Some(Index(6)))
  }
}
