package mcts

import mcts.games.Red
import mcts.solver.{MonteCarlo, Node}

import scala.annotation.tailrec
import scala.concurrent.duration._

object SolverInterface {

  /**
    * This uses multiple groups of parameters to guide type inference
    */
  // format: off
  def apply[State, Action, Player](game:     Game[State, Action, Player],
                                   duration: Duration)
                                  (runner:   Runner[Node[Action]])
                         (implicit renderer: Renderer[State, fansi.Str]) =
  // format: on
  step(
    solver    = new MonteCarlo(game),
    duration  = duration,
    runner    = runner,
    state     = game.startingState,
    turn      = 0,
    summaries = Nil,
    renderer  = renderer
  )

  @tailrec
  private def step[State, Action, Player](
      solver:    MonteCarlo[State, Action, Player],
      duration:  Duration,
      runner:    Runner[Node[Action]],
      state:     State,
      turn:      Int,
      summaries: List[fansi.Str],
      renderer:  Renderer[State, fansi.Str]
  ): Unit = {

    //reset terminal
    println("\u001b[2J")
    //move cursor to start of screen
    println("\u001b[H")

    println(renderer(state))
    summaries.take(10).foreach(println)

    solver.game.gameResult(state) match {
      case Draw =>
        println(fansi.Bold.On("It's a Tie!"))

      case Winner(winner) =>
        println(fansi.Bold.On(s"$winner won"))

      case Ongoing(_) =>
        val newNode      = solver.nextAction(state, duration, runner)
        val player       = solver.game.currentPlayer(state)
        val chosenAction = newNode.bestAction
        val nextState    = solver.game.nextState(chosenAction, state)
        val summary      = summarize(turn, newNode, player, chosenAction)

        step(solver, duration, runner, nextState, turn + 1, summary :: summaries, renderer)
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
    color(s"[$turn] chose $chosenAction after ${newNode.numPlays} simulations. $winnerChance chance of win")
  }
}
