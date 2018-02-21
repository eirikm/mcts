package mcts

import fansi.{Color,              Str}
import mcts.data.Array2d.{Column, Row}
import mcts.games.ConnectFour.LastAction
import mcts.games._

trait Renderer[State, Output] {
  def apply(state: State): Output
}

object renderers {
  def renderRedBlue(player: RedBlue, isPartOfVictory: Boolean): Str =
    player match {
      case Red  => Color.Red(if (isPartOfVictory) "*" else "◍")
      case Blue => Color.Blue(if (isPartOfVictory) "*" else "●")
    }

  def renderSpace(space: Space, isPartOfVictory: Boolean): Str =
    space match {
      case Occupied(player) => renderRedBlue(player, isPartOfVictory)
      case Empty            => "◌"
    }

  implicit object RenderConnectFour extends Renderer[ConnectFour.State, Str] {
    /* show column number or an arrow indicating last drop */
    def col(state: ConnectFour.State)(column: Column): Str =
      state.lastActionOpt match {
        case Some(LastAction(Red,  `column`, _)) => Color.Red(" ⇓ ")
        case Some(LastAction(Blue, `column`, _)) => Color.Blue(" ⇓ ")
        case _ => s" $column "
      }

    /* When the game is won, render winning cells as asterisks */
    def isPartOfVictory(state: ConnectFour.State, col: Column, row: Row): Boolean =
      state.winnerOpt match {
        case Some((_, line)) => line.contains(state.board.index(col, row))
        case _ => false
      }

    def row(state: ConnectFour.State)(row: Row): String =
      0.until(ConnectFour.NumCols)
        .map(col => renderSpace(state.board(col, row), isPartOfVictory(state, col, row)))
        .mkString("│ ", " │ ", " │")

    override def apply(state: ConnectFour.State): Str = {
      val h1     = 0.until(ConnectFour.NumCols).map(_ => "═══").mkString("╔", "╤",  "╗\n")
      val h2     = 0.until(ConnectFour.NumCols).map(col(state)).mkString("║", "│",  "║\n")
      val h3     = 0.until(ConnectFour.NumCols).map(_ => "═══").mkString("╠", "╪",  "╣\n")
      val footer = 0.until(ConnectFour.NumCols).map(_ => "═══").mkString("╚", "╧",  "╝\n")
      val table  = 0.until(ConnectFour.NumRows).map(row(state)).mkString("",  "\n", "\n")

      h1 ++ h2 ++ h3 ++ table ++ footer
    }
  }

  implicit object RenderTicTacToe extends Renderer[TicTacToe.State, Str] {
    /* When the game is won, render winning cells as asterisks */
    def isPartOfVictory(state: TicTacToe.State, col: Column, row: Row): Boolean =
      state.winnerOpt match {
        case Some((_, line)) => line.contains(state.grid.index(col, row))
        case _ => false
      }

    def row(state: TicTacToe.State)(row: Row): String =
      0.until(3)
        .map(col => renderSpace(state.grid(col, row), isPartOfVictory(state, col, row)))
        .mkString("│ ", " │ ", " │")

    override def apply(state: TicTacToe.State): Str = {
      val h1     = 0.until(3).map(_ => "═══").mkString("╔", "╤",  "╗\n")
      val footer = 0.until(3).map(_ => "═══").mkString("╚", "╧",  "╝\n")
      val table  = 0.until(3).map(row(state)).mkString("",  "\n", "\n")
      h1 ++ table ++ footer
    }
  }
}
