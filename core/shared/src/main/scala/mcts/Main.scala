package mcts

import fansi.Str
import mcts.engines.{MonteCarlo, Node, RunPar}
import mcts.games.Red
import mcts.renderers._

import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.io.StdIn
import scala.reflect.ClassTag

object Main {
  // format: off
  @tailrec
  private def step[State, Action: ClassTag, Player]
                  (game:      Game[State, Action, Player],
                   duration:  Duration)
                  (state:     State           = game.startingState,
                   turn:      Int             = 0,
                   summaries: List[fansi.Str] = Nil)
         (implicit renderer:  Renderer[State, fansi.Str]): Unit = {
    // format: on

    println("\u001b[2J")
    println("\u001b[H")
    println(renderer(state))
    summaries.take(10).foreach(println)

    game.gameResult(state) match {
      case Draw =>
        println(fansi.Bold.On("It's a Tie!"))
        if (Console.in != null) StdIn.readLine()

      case Winner(winner) =>
        println(fansi.Bold.On(s"$winner won"))
        if (Console.in != null) StdIn.readLine()

      case Ongoing(_) =>
        val solver = new MonteCarlo(game)
//        val runner = new RunSeq(solver)
        val runner       = new RunPar(solver, parallelism = 6)
        val newNode      = runner.timedMCTS(duration, state)
        val player       = game.currentPlayer(state)
        val chosenAction = newNode.bestAction
        val nextState    = game.nextState(chosenAction, state)
        val summary: Str = summarize(turn, newNode, player, chosenAction)

        step(game, duration)(nextState, turn + 1, summary :: summaries)
    }
  }

  private def summarize[Player, Action, State](
      turn:         Int,
      newNode:      Node[Action],
      player:       Player,
      chosenAction: Action
  ): fansi.Str = {
    val color        = if (player == Red) fansi.Color.Red else fansi.Color.Blue
    val winnerChance = "%,.2f".format(newNode.numWins.toDouble / newNode.numPlays)
    color(s"[$turn] chose $chosenAction after ${newNode.numPlays} simulations. $winnerChance")
  }

  def main(args: Array[String]): Unit = {
    step(games.TicTacToe.GameDef,   100.millis)()
    step(games.ConnectFour.GameDef, 2000.millis)()
  }
}
