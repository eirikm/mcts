package mcts

trait Runner[T] {
  def apply(once: () => T): T
}

object Runner {
  def Sequential[T]: Runner[T] = (once: () => T) => once()

  def MapReduce[T](combine: (T, T) => T): Runner[T] =
    (once: () => T) => {
      val parallelism = math.max(Runtime.getRuntime.availableProcessors() - 1, 1)

      (0 until parallelism).par
        .map(_ => once())
        .seq
        .reduce(combine)
    }
}
