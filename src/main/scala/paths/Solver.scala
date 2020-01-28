package paths

import scala.annotation.tailrec

class Solver(m: Int, n: Int) extends Grid(m, n) {

  private def neighborsWithHistory(pos: Pos, history: List[Move]): List[PartialPath] =
    for ( ln <- pos.legalNeighbors.to(List) ) yield (ln._1, ln._2 :: history)

  /**
   * Would it be useful to consider as a move to do an elementary expansion or contraction?
   * If so, then one could perhaps use the GameDef regime to create new chains from old ones
   * instead of the brute-force for-comprehension that I use.
    */
  // def elementaryExpansions(b: Block, history: List[Move]): List[PartialPath] = ???

  private def paths(initial: List[PartialPath]): List[PartialPath] = {
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

  private def pathsToGoal =
    paths(List(init)).filter(path => done(path._1)).map(pp => pp._2)

  def pathsGroupedByMinimality =
    pathsToGoal.groupBy(p => if (pathLength(p) == minimalLength) "minimal" else "non-minimal")

  private def pathsGroupedByLength = pathsToGoal.groupBy(p => pathLength(p))


  type PathChain = List[Path]
  def canGrow(pc: PathChain) = !isMinimal(pc.head)

  /**
   * A brute-force method that takes viables and non-viables - meaning path chains that can either
   * be extended or not. The viable path chains are input as the value initial. Based on initial,
   * we obtain more, which is then split into viable and non-viable. Eventually, everything
   * ends up in gather. The recursion ends when there are no more viables, in which case we
   * return what is at that time in gather.
   *
   * I have previously done this more effectively in C and I think I then created the chains of paths
   * simultaneously instead of brute-force comparing paths after constructing all of them.
   *
   * @param initial is a list of PathChain's that are viable - meaning that they can be extended
   * @param gather is a list of PathChain's that are non-viable - meaning that they cannot be extended
   * @return the list of PathChain's that
   */
  @tailrec
  private def chains(initial: List[PathChain], gather: List[PathChain]): List[PathChain] = initial match {
    case List() => gather
    case _ => {
      val more = for {
        pc <- initial
        (key, value) <- pathsGroupedByLength
        path <- value
        if (key < pathLength(pc.head) && pathLessThan(path, pc.head))
      } yield path :: pc

      val grouped = more.groupBy(pc => if (canGrow(pc)) "viable" else "non-viable")

      val viable = grouped.get("viable") match {
        case Some(value) => value
        case None => List()
      }

      val nonViable = grouped.get("non-viable") match {
        case Some(value) => value
        case None => List()
      }

      chains(viable, nonViable ::: initial ::: gather)
    }
  }

  private def chainsByLength = {
    val grouped = pathsGroupedByMinimality

    chains(grouped("non-minimal").map(p => List(p)), grouped("minimal").map(p => List(p))).
      map(pc => pc.length - 1).groupBy(identity).
      map{case (key, value) => (key, value.length)}
  }

  def eulercharacteristic: Int =
    chainsByLength.
      map{case (key, value) => if (key % 2 == 0) (key, value) else (key, -value)}.
      values.
      reduce(_ + _)
}

