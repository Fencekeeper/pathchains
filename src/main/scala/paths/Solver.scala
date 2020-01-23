package paths

/**
 * In my master thesis, I have a calculation of the number of (m, n)-paths.
 * I can write a test that ensures that the number of 0-simplices
 * is correct, then.
 */

/**
 * Should I use the list of moves to retrace the steps from goal to startPos.
 * But then I might have to reimplement deltaRow etc so that they detract 1
 * instead of adding 1.
 */

/**
 * How do I use the paths to obtain chains? Well, all paths but the
 * maximal ones would be expanded. However, I can't just consider
 * chains in which each step is an elementary expansion. That would
 * not yield all chains.
 */

class Solver(m: Int, n: Int) extends Grid(m, n) {

  def neighborsWithHistory(b: Block, history: List[Move]): LazyList[PartialPath] =
    for ( ln <- b.legalNeighbors.to(LazyList) ) yield (ln._1, ln._2 :: history)

  // Would this be useful?
  // def elementaryExpansions(b: Block, history: List[Move]): LazyList[PartialPath] = ???

  def paths(initial: LazyList[PartialPath]): LazyList[PartialPath] = {
    if (initial.isEmpty)
      LazyList.empty
    else {
      val more = for {
        path <- initial // traverses through partial paths
        neighbor <- neighborsWithHistory(path._1, path._2) // traverses through neighbors
      } yield neighbor

      initial #::: paths(more)
    }
  }

  def init: PartialPath = (Block(startPos), List.empty)

  lazy val pathsToGoal = paths(LazyList(init)).filter(path => done(path._1))

  def pathsGrouped(ps: LazyList[PartialPath]) =
    ps.groupBy(length)

  /**
   * Actually, this type is somewhat misleading as there must be order.
   * Does it make sense to use LazyList? Maybe not, because
   * if I do it as naively as below, then I would have to compare
   * paths to put them into chains.
   */
  type PathChain = LazyList[PartialPath]

  // def chains(initial: LazyList[PathChain]): LazyList[PathChain] = ???
}
