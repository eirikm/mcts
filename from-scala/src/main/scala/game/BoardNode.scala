package game

sealed trait Player extends BoardNode {
  def other: Player =
    this match {
      case PlayerTwo => PlayerOne
      case PlayerOne => PlayerTwo
    }
}

sealed trait BoardNode

case object Empty extends BoardNode

case object PlayerOne extends Player

case object PlayerTwo extends Player

sealed trait Result
case object Draw extends Result
case class Winner(player: Player) extends Result
