package mcts

import fansi.Str

object Util {

  /**
    * @param map Assumed to be non-empty
    */
  def maxBy[K, V](map: Map[K, V])(f: V => Double): K = {
    var maxK:     K      = map.head._1
    var maxValue: Double = Double.MinValue
    map.keys.foreach { k =>
      val value = f(map(k))
      if (maxValue < value) {
        maxK     = k
        maxValue = value
      }
    }

    maxK
  }

  def lines(str: Str): List[Str] = {
    def go(str: Str, ret: List[Str]): List[Str] = {
      var idx = 0
      while (idx < str.length) {
        if (str.getChar(idx) == '\n') {
          return go(str.substring(idx + 1), str.substring(0, idx) :: ret)
        }
        idx += 1
      }
      str :: ret
    }

    go(str, Nil).reverse
  }
}
