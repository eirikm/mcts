package mcts.games

import mcts.data.Array2d
import mcts.data.Array2d.Index
import mcts._
import mcts.solver.MonteCarlo
import org.scalatest.{FunSuite, Matchers}

import scala.concurrent.duration._

class TicTacToeTest extends FunSuite with Matchers {
  import mcts.games.TicTacToe.GameDef

  val solver = new MonteCarlo(GameDef)

  test("should always favor the winning move in a game of Tic Tac Toe") {
    val actions = 0.until(10).map { _ =>
      // format: off
        val board =
          Board(
            Occupied(Blue), Occupied(Blue), Empty,
            Occupied(Red), Occupied(Red), Empty,
            Occupied(Blue), Occupied(Blue), Empty
          )
        // format: on
      solver.nextAction(GameDef.startingState.copy(grid = board), 50.milli, Runner.Sequential).bestAction
    }

    actions.toSet should equal(Set(Index(5)))
  }

  test("should always block the winning move in a game of Tic Tac Toe") {
    val actions = 0.until(10).map { _ =>
      val board =
        // format: off
          Board(
            Empty, Empty, Occupied(Blue),
            Empty, Occupied(Blue), Empty,
            Empty, Empty, Empty
          )
        // format: on
      solver.nextAction(GameDef.startingState.copy(grid = board), 50.milli, Runner.Sequential).bestAction
    }

    actions.toSet should equal(Set(Index(6)))
  }

  test("A newly created State should just have an 3 x 3 board.") {
    GameDef.startingState.grid.array.length should equal(9)
  }

  test("A newly created State should be completely empty.") {
    GameDef.startingState.grid.array.count(x => x != Empty) should equal(0)
  }

  test("A newly created State should have its lastPlayerWhoMoved value as player 1.") {
    GameDef.startingState.currentPlayer should be(Red)
  }

  test("A newly created State should have all indices [0..8] as available actions.") {
    GameDef.gameResult(GameDef.startingState) match {
      case Ongoing(actions) => actions should equal(Array(0, 1, 2, 3, 4, 5, 6, 7, 8))
      case other            => fail(other.toString)
    }
  }

  test("A newly created State should render correctly") {
    val expected = """|╔═══╤═══╤═══╗
                      |│ ◌ │ ◌ │ ◌ │
                      |│ ◌ │ ◌ │ ◌ │
                      |│ ◌ │ ◌ │ ◌ │
                      |╚═══╧═══╧═══╝
                      |""".stripMargin
    renderers.RenderTicTacToe(GameDef.startingState).plainText should equal(expected)
  }

  test("After player 1 makes a move on index=0, State should have correct string representation.") {
    val state = GameDef.nextState(0, GameDef.startingState.copy(currentPlayer = Blue))

    val expected = """|╔═══╤═══╤═══╗
                      |│ ● │ ◌ │ ◌ │
                      |│ ◌ │ ◌ │ ◌ │
                      |│ ◌ │ ◌ │ ◌ │
                      |╚═══╧═══╧═══╝
                      |""".stripMargin

    renderers.RenderTicTacToe(state).plainText should equal(expected)
  }

  test(
    "After player 1 makes a move on index=0, and player 2 makes a move on index=8, State should have correct string representation."
  ) {
    val state    = GameDef.nextState(8, GameDef.nextState(0, GameDef.startingState.copy(currentPlayer = Blue)))
    val expected = """|╔═══╤═══╤═══╗
                      |│ ● │ ◌ │ ◌ │
                      |│ ◌ │ ◌ │ ◌ │
                      |│ ◌ │ ◌ │ ◍ │
                      |╚═══╧═══╧═══╝
                      |""".stripMargin
    renderers.RenderTicTacToe(state).plainText should equal(expected)
  }

  test("should recognize red as winner in") {
    // format: off
    val board = Board(
      Occupied(Red),  Occupied(Red),  Occupied(Red),
      Occupied(Blue), Occupied(Blue), Occupied(Red),
      Occupied(Red),  Occupied(Blue), Occupied(Red)
    )
    // format: on
    assert(GameDef.gameResult(GameDef.startingState.copy(grid = board)) == Winner(Red))
  }

  test("should recognize blue as winner in") {
    // format: off
    val grid = Board(
      Occupied(Blue), Occupied(Red),  Occupied(Blue),
      Occupied(Blue), Occupied(Blue), Occupied(Red),
      Occupied(Blue), Occupied(Red),  Occupied(Red)
    )
    // format: on
    assert(GameDef.gameResult(GameDef.startingState.copy(grid = grid)) == Winner(Blue))
  }

  test("should recognize draw") {
    // format: off
    val grid = Board(
      Occupied(Blue), Occupied(Red), Occupied(Blue),
      Occupied(Red), Occupied(Blue), Occupied(Red),
      Occupied(Red), Occupied(Blue), Occupied(Red)
    )
    // format: on
    assert(GameDef.gameResult(GameDef.startingState.copy(grid = grid)) == Draw)
  }

  def Board(s0: Space, s1: Space, s2: Space, s3: Space, s4: Space, s5: Space, s6: Space, s7: Space, s8: Space)
    : Array2d[Space] = {
    val ret = Array2d[Space](3, 3, Empty)
    ret.array(0) = s0
    ret.array(1) = s1
    ret.array(2) = s2
    ret.array(3) = s3
    ret.array(4) = s4
    ret.array(5) = s5
    ret.array(6) = s6
    ret.array(7) = s7
    ret.array(8) = s8
    ret
  }
}
