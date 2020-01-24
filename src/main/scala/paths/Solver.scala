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

  lazy val pathsToGoal =
    paths(LazyList(init)).filter(path => done(path._1))

  def pathsGrouped(ps: LazyList[PartialPath]) =
    ps.groupBy(pathLength)

  /**
   * Actually, this type is somewhat misleading as there must be order.
   * Does it make sense to use LazyList? Maybe not, because
   * if I do it as naively as below, then I would have to compare
   * paths to put them into chains.
   */
  type PathChain = LazyList[PartialPath]

  // The plan is to use all paths except the ones of length zero as initial and then the empty list
  // as buildup. Because I managed to make the function tail-recursive, this should not
  // blow the stack. What about the heap?
  def chains(initial: LazyList[PathChain], gather: LazyList[PathChain]): LazyList[PathChain] = initial match {
    case LazyList() => gather
    case _ => {
      val more = for {
        pc <- initial
        (key, value) <- pathsGrouped(pathsToGoal)
        path <- value
        if (key < pathLength(pc.head) && pathLessThan(path, pc.head))
      } yield path #:: pc

      chains(more, initial #::: gather)
    }
  }

  lazy val eulercharacteristic = {
    val grouped = pathsToGoal.groupBy(p => if (pathLength(p) == 0) "zero" else "non-zero")

    chains(pathsToGoal.filter(p => pathLength(p) > 0).map(p => LazyList(p)), pathsToGoal.filter(p => pathLength(p) == 0).map(p => LazyList(p))).
      map(pc => pc.length).groupBy(identity).
      map{case (key, value) => (key, value.length)}.
      map{case (key, value) => if (key % 2 == 0) (key, value) else (key, -value)}/*.
      values.
      reduce(_ + _)*/
  }
}
