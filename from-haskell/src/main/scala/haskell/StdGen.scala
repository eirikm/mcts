package haskell

trait StdGen {
  def nextInt: (Int, StdGen)

  final def nextInt(bound: Int): (Int, StdGen) = {
    val (i, next) = nextInt
    (i % bound, next)
  }

  final def oneFrom[T](ts: Seq[T]): (T, StdGen) = {
    val (idx, next) = nextInt(ts.size)
    (ts(idx), next)
  }
}

object StdGen {
  def simple(seed: Long): StdGen = new StdGen {
    override def nextInt: (Int, StdGen) = {
      val seed2 = (seed * 0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
      val ret   = math.abs((seed2 >>> 16).toInt)
      (ret, simple(seed2))
    }
  }
}
