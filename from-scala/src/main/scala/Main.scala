import game._
import mcts.UCT

/**
  * Created by culim on 2/24/16.
  */
object Main extends App {
  var state = HexState(7, 7)

  while (state.getAvailableActions.nonEmpty) {
    val currentPlayer = state.getLastPlayerWhoMoved.other
    println(s"Player $currentPlayer's turn.")
    println(state.toString)

    val action: Int = UCT.search(state, 200)

    println(s"Player $currentPlayer's best action is $action\n")
    state = state.doAction(action)
  }

  println(state.toString)

  state.getResult match {
    case Draw => println(s"It's a draw!")
    case Winner(player) =>
      val who = player match {
        case PlayerOne => "1"
        case PlayerTwo => "2"
      }
      println(s"Aha! Player $who wins!")
  }
}
