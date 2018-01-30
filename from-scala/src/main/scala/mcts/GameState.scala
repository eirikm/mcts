package mcts

import game.{Player, Result}

/**
  * Created by culim on 2/24/16.
  */
trait GameState {
  type Self <: GameState

  def getLastPlayerWhoMoved: Player

  def getAvailableActions: Set[Int]

  def getResult: Result

  def doAction(action: Int): Self
}


