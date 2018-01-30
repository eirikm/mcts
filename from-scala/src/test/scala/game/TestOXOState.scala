package game

import org.scalatest.{FunSuite, Matchers}

/**
  * Created by culim on 2/24/16.
  */
class TestOXOState extends FunSuite with Matchers {

  val fresh: OXOState = new OXOState

  test("A newly created OXOState should just have an 3 x 3 board.") {
    fresh.board.length should equal(9)
  }

  test("A newly created OXOState should be completely empty.") {
    fresh.board.count(x => x != Empty) should equal(0)
  }

  test("A newly created OXOState should have its lastPlayerWhoMoved value as player 2.") {
    fresh.getLastPlayerWhoMoved should be (PlayerTwo)
  }

  test("A newly created OXOState should yield a result of zero for both player 1 and 2.") {
    fresh.getResult should be (Draw)
  }

  test("A newly created OXOState should have all indices [0..8] as available actions.") {
    println(fresh.getPlayerInWinConditions)
    fresh.getAvailableActions should be (Set(0, 1, 2, 3, 4, 5, 6, 7, 8))
  }

  test("A newly created OXOState should have a correct toString() representation.") {
    fresh.toString should equal ("...\n...\n...\n")
  }

  test("After player 1 makes a move on index=0, OXOState should have correct string representation.") {
    var state: OXOState = fresh
    state = state.doAction(0)
    state.toString should equal("O..\n...\n...\n")
  }

  test("After player 1 makes a move on index=0, and player 2 makes a move on index=8, OXOState should have correct string representation.") {
    var state: OXOState = fresh
    state = state.doAction(0)
    state = state.doAction(8)
    state.toString should equal("O..\n...\n..X\n")
  }

  test("The result of a player-winning board should be player1=1.0, player2=0.0.") {
    val board = Array[BoardNode](
      PlayerOne, PlayerOne, PlayerOne,
      PlayerTwo, PlayerTwo, PlayerOne,
      PlayerOne, PlayerTwo, PlayerOne
    )
    assert(new OXOState(PlayerOne, board).getResult == Winner(PlayerOne))
  }

  test("The result of a cpu-winning board should be player1=0.0, player2=1.0.") {
    val board = Array[BoardNode](
      PlayerTwo, PlayerOne, PlayerTwo,
      PlayerTwo, PlayerTwo, PlayerOne,
      PlayerTwo, PlayerOne, PlayerOne
    )

    assert(new OXOState(PlayerTwo, board).getResult == Winner(PlayerTwo))
  }

  test("The result of a drawn board should be player1=0.5, player2=0.5.") {
    val board = Array[BoardNode](
      PlayerTwo, PlayerOne, PlayerTwo,
      PlayerOne, PlayerTwo, PlayerOne,
      PlayerOne, PlayerTwo, PlayerOne
    )

    assert(new OXOState(PlayerTwo, board).getResult == Draw)
  }
}
