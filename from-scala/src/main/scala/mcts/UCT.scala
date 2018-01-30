package mcts

import scala.util.Random

/**
  * Created by culim on 2/24/16.
  */
object UCT {
  def search(rootState: GameState, maxIterations: Int, verbose: Boolean = false): Int = {
    val rootNode = GameNode(-1, state = Some(rootState), parent = None)

    for (iteration <- 1 to maxIterations) {
      var node: GameNode = rootNode
      var state: GameState = rootState

      // Select
      while (node.untriedActions.isEmpty && node.children.nonEmpty) {
        // Node has exhausted all actions -- fully expanded
        // Node is non-terminal -- still has children to explore
        node = node.selectChild.orNull
        state = state.doAction(node.action)
      }

      // Expand
      if (node.untriedActions.nonEmpty) {
        val action: Int = node.untriedActions.toList(Random.nextInt(node.untriedActions.size))
        state = state.doAction(action)
        node.addChild(action, state)
      }

      // Rollout
      while (state.getAvailableActions.nonEmpty) {
        state = state.doAction(state.getAvailableActions.toList(Random.nextInt(state.getAvailableActions.size)))
      }

      // Backpropagate

      while (node != null) {
        node.update(state.getResult)
        node = node.parent.orNull
      }
    }

    if (verbose) {
      println(rootNode.treeToString(0))
    }
    else {
      println(rootNode.childrenToString)
    }

    rootNode.children.maxBy(_.numberOfVisits).action
  }
}
