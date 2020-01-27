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

  def neighborsWithHistory(pos: Pos, history: List[Move]): List[PartialPath] =
    for ( ln <- pos.legalNeighbors.to(List) ) yield (ln._1, ln._2 :: history)

  // Would this be useful?
  // def elementaryExpansions(b: Block, history: List[Move]): List[PartialPath] = ???

  def paths(initial: List[PartialPath]): List[PartialPath] = {
    if (initial.isEmpty)
      List()
    else {
      val more = for {
        path <- initial // traverses through partial paths
        neighbor <- neighborsWithHistory(path._1, path._2) // traverses through neighbors
      } yield neighbor

      initial ::: paths(more)
    }
  }

  def init: PartialPath = (startPos, List())

  lazy val pathsToGoal =
    paths(List(init)).filter(path => done(path._1)).map(pp => pp._2)

  def pathsGroupedByMinimality =
    pathsToGoal.groupBy(p => if (pathLength(p) == minimalLength) "minimal" else "non-minimal")

  lazy val pathsGroupedByLength = pathsToGoal.groupBy(p => pathLength(p))


  /**
   * Actually, this type is somewhat misleading as there must be order.
   * Does it make sense to use LazyList? Maybe not, because
   * if I do it as naively as below, then I would have to compare
   * paths to put them into chains.
   */
  type PathChain = List[Path]
  def canGrow(pc: PathChain) = !isMinimal(pc.head)

  // The plan is to use all paths except the ones of length zero as initial and then the empty list
  // as buildup. Because I managed to make the function tail-recursive, this should not
  // blow the stack. What about the heap?
  def chains(initial: List[PathChain], gather: List[PathChain]): List[PathChain] = initial match {
    case List() => gather
    case _ => {
      val more = for {
        pc <- initial
        (key, value) <- pathsGroupedByLength
        path <- value
        if (key < pathLength(pc.head) && pathLessThan(path, pc.head))
      } yield path :: pc

      println("\nInside chains: initial.head = " + initial.head + " ...and ")

      val grouped = more.groupBy(pc => if (canGrow(pc)) "viable" else "non-viable")

      val viable = grouped.get("viable") match {
        case Some(value) => value
        case None => List()
      }

      val nonViable = grouped.get("non-viable") match {
        case Some(value) => value
        case None => List()
      }

      /**
       * The following printouts clearly show that the for-comprehension
       * is not working as intended. As of friday 24th of January,
       * at 15:00, I've determined that the function pathLessThan
       * does not work
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

      chains(viable, nonViable ::: initial ::: gather)
    }
  }

  /**
   * I have an off by one-error. Seems I calculate chain-length as 1 too high.
   */
  lazy val eulercharacteristic = {
    val grouped = pathsGroupedByMinimality

    println("The result of ...")
    println(pathLessThan(List(Diagonally), List(Up, Right)))
    println("...applying pathLessThan on List(Diagonally) and List(Up, Right).")

    chains(grouped("non-minimal").map(p => List(p)), grouped("minimal").map(p => List(p))).
      map(pc => pc.length - 1).groupBy(identity).
      map{case (key, value) => (key, value.length)}.
      map{case (key, value) => if (key % 2 == 0) (key, value) else (key, -value)}/*.
      values.
      reduce(_ + _)*/
  }
}

