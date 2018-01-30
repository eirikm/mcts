package game

import org.scalatest.FunSuite

import scala.util.Random

/**
  * Created by culim on 2/24/16.
  */
class TestHexState extends FunSuite {

  test("A newly created HexState should just have a non-empty board.") {
    val nRows: Int = 11
    val nColumns: Int = 11
    val state: HexState = HexState(nRows, nColumns)

    assert(state.board.length == nRows * nColumns)
  }

  test("A 5 x 5 board where player 1 picks (0,a) should be valid.") {
    val nRows: Int = 5
    val nColumns: Int = 5
    var state: HexState = HexState(nRows, nColumns)

    state = state.doAction(0)

    assert(state.board(0) == PlayerOne, "Player 1 should be in position (a,0)")
  }

  test("A 5 x 5 board where player 1 picks (0,a) and player 2 picks (0, b) should be valid.") {
    val nRows: Int = 5
    val nColumns: Int = 5
    var state: HexState = HexState(nRows, nColumns)

    state = state.doAction(0)
    state = state.doAction(1)

    assert(state.board(0) == PlayerOne, "Player 1 should be in position (a,0)")
    assert(state.board(1) == PlayerTwo, "Player 2 should be in position (a,0)")
  }

  test("P1 and P2 alternate across 5 turns should reduce #available_actions.") {
    val nRows: Int = 5
    val nColumns: Int = 5
    var state = HexState(nRows, nColumns)

    val nTurns = 5
    for (i <- 1 to nTurns) {
      state = state.doAction(state.getAvailableActions.toList(Random.nextInt(state.getAvailableActions.size)))
      println(state)
      state = state.doAction(state.getAvailableActions.toList(Random.nextInt(state.getAvailableActions.size)))
      println(state)

    }
    assert(state.getAvailableActions.size == nRows * nColumns - nTurns * 2)
  }

  test("P1 in winning configuration has won should be detected.") {
    val nRows: Int = 5
    val nColumns: Int = 5
    val board = Array[BoardNode](
      PlayerOne, PlayerOne, Empty,     Empty,     Empty,
      Empty,     PlayerOne, PlayerOne, Empty,     Empty,
      Empty,     Empty,     PlayerOne, Empty,     Empty,
      Empty,     Empty,     PlayerOne, PlayerOne, PlayerOne,
      Empty,     Empty,     PlayerTwo, PlayerTwo, PlayerTwo
    )

    val state = HexState(nRows, nColumns, PlayerOne, board)

    val actual: BoardNode = state.getPlayerInWinConditions._1
    assert(PlayerOne == actual)
  }

  test("P2 in winning configuration has won should be detected.") {
    val nRows: Int = 5
    val nColumns: Int = 5
    val board = Array[BoardNode](
      PlayerOne, Empty,     PlayerTwo, Empty,     PlayerTwo,
      Empty,     PlayerTwo, PlayerOne, PlayerTwo, Empty,
      Empty,     Empty,     PlayerTwo, PlayerOne, Empty,
      Empty,     PlayerTwo, PlayerOne, Empty,     Empty,
      PlayerTwo, PlayerOne, PlayerOne, PlayerOne, Empty
    )
    val state: HexState = HexState(nRows, nColumns, PlayerTwo, board)

    assert(PlayerTwo == state.getPlayerInWinConditions._1)
  }

  test("P2 in making final move on 7x7 board should not result in a win.") {
    val nRows: Int = 7
    val nColumns: Int = 7

    val board = Array[BoardNode](
      PlayerOne, PlayerOne, PlayerOne, PlayerOne, PlayerTwo, PlayerTwo, PlayerOne,
      PlayerOne, PlayerOne, Empty, PlayerTwo, Empty, Empty, PlayerOne,
      PlayerOne, PlayerTwo, PlayerTwo, PlayerOne, Empty, Empty, Empty,
      PlayerTwo, PlayerOne, PlayerTwo, PlayerOne, Empty, Empty, Empty,
      PlayerTwo, PlayerTwo, PlayerOne, PlayerTwo, PlayerOne, PlayerTwo, Empty,
      PlayerTwo, PlayerOne, PlayerOne, PlayerOne, Empty, PlayerTwo, PlayerTwo,
      PlayerOne, PlayerOne, Empty, PlayerTwo, PlayerOne, PlayerTwo, PlayerTwo
    )
    var state: HexState = HexState(nRows, nColumns, PlayerOne, board)

    var pair = state.getPlayerInWinConditions
    assert(pair._1 == Empty)

    state = state.doAction(39)

    pair = state.getPlayerInWinConditions

    println(state)
    println(pair._2.mkString(","))
    assert(pair._1 == Empty)
  }
}
