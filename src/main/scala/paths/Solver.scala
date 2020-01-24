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

  def pathsGroupedByMinimality =
    pathsToGoal.groupBy(p => if (pathLength(p) == minimalLength) "minimal" else "non-minimal")

  lazy val pathsGroupedByLength = pathsToGoal.groupBy(p => pathLength(p))


  /**
   * Actually, this type is somewhat misleading as there must be order.
   * Does it make sense to use LazyList? Maybe not, because
   * if I do it as naively as below, then I would have to compare
   * paths to put them into chains.
   */
  type PathChain = LazyList[PartialPath]
  def canGrow(pc: PathChain) = !isMinimal(pc.head)

  // The plan is to use all paths except the ones of length zero as initial and then the empty list
  // as buildup. Because I managed to make the function tail-recursive, this should not
  // blow the stack. What about the heap?
  def chains(initial: LazyList[PathChain], gather: LazyList[PathChain]): LazyList[PathChain] = initial match {
    case LazyList() => gather
    case _ => {
      val more = for {
        pc <- initial
        (key, value) <- pathsGroupedByLength
        path <- value
        if (key < pathLength(pc.head) && pathLessThan(path, pc.head))
      } yield path #:: pc

      val grouped = more.groupBy(pc => if (canGrow(pc)) "viable" else "non-viable")

      val viable = grouped.get("viable") match {
        case Some(value) => value
        case None => LazyList()
      }

      val nonViable = grouped.get("non-viable") match {
        case Some(value) => value
        case None => LazyList()
      }

      /**
       * The following printouts clearly show that the for-comprehension
       * is not working as intended.
       */

      println("\ngather:")
      gather.foreach(println)
      println("\ninitial:")
      initial.foreach(println)
      println("\nmore:")
      more.foreach(println)
      println("\nnon-viable:")
      nonViable.foreach(println)
      println("\nviable:")
      viable.foreach(println)

      chains(viable, nonViable #::: initial #::: gather)
    }
  }

  lazy val eulercharacteristic = {
    val grouped = pathsGroupedByMinimality

    chains(grouped("non-minimal").map(p => LazyList(p)), grouped("minimal").map(p => LazyList(p))).
      map(pc => pc.length).groupBy(identity).
      map{case (key, value) => (key, value.length)}.
      map{case (key, value) => if (key % 2 == 0) (key, value) else (key, -value)}/*.
      values.
      reduce(_ + _)*/
  }
}

