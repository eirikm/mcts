package mcts

import game.{Result, Winner}

import scala.collection.mutable.ListBuffer

object GameNode {
  val epsilon: Double = 1e-6

  def valueOf(numberOfVisits: Int)(node: GameNode): Double =
    (node.numberOfWins / node.numberOfVisits) +
      Math.sqrt(2 * Math.log(numberOfVisits + 1) / (node.numberOfVisits + epsilon))
}
/**
  * Created by culim on 2/24/16.
  */
case class GameNode(action: Int, parent: Option[GameNode], state: Option[GameState]) {
  var numberOfWins: Double = 0
  var numberOfVisits: Int = 0
  var children: ListBuffer[GameNode] = ListBuffer.empty
  var untriedActions: Set[Int] = state.fold(Set.empty[Int])(_.getAvailableActions)

  def selectChild: Option[GameNode] =
    children.sortBy(GameNode.valueOf(numberOfVisits)).lastOption

  def update(result: Result): Unit = {
    numberOfVisits += 1
    result match {
      case Winner(_) => numberOfWins += 1
      case _ => ()
    }
  }

  def addChild(action: Int, state: GameState): GameNode = {
    val n = GameNode(action, Some(this), Some(state))
    untriedActions -= action
    children += n

    n
  }

  override def toString: String = {
    s"[A: $action; " +
      s"W/V: $numberOfWins/$numberOfVisits = ${numberOfWins / numberOfVisits}; " +
      s"U: $untriedActions"
  }

  def treeToString(indent: Int): String = {
    var s: String = indentString(indent) + this.toString
    for (c <- children) {
      s += c.treeToString(indent + 1)
    }
    s
  }

  def indentString(indent: Int): String = {
    var s = "\n"
    for (i <- 1 to indent) {
      s += "| "
    }
    s
  }


  def childrenToString: String =
    children.map(_.toString).mkString("\n")
}
