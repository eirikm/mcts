package mcts

object Main extends App {
  val mcts = new MCTS(TicTacToeGame(), rounds = 1000, new util.Random(0L))

  while (mcts.game.winner.isEmpty) {
    mcts.selectMove match {
      case Some(nextMove) =>
        println(s"Current player: ${mcts.game.currentPlayer}. Next move: $nextMove. Board: ${mcts.game.board}")
        mcts.performMove(nextMove)
      case None =>
        println(s"Winner is: ${mcts.game.winner.get}")
        println(mcts.game.board)
    }
  }
}
