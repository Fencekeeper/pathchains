package paths

/**
 * Recall that we're only interested in the case when m >= n.
 */
class Grid(m: Int, n: Int) extends GameDef {

  lazy val startPos = Pos(0, 0)
  lazy val goal: Pos = Pos(m, n)
  def dimensions = (m, n)

  override def terrain: Terrain =
    p => startPos <= p && p <= goal

  type PartialPath = (Pos, List[Move])
  type Path = List[Move]

  def pathLength(p: Path): Int = p.length
  def pathLength(p: PartialPath): Int = pathLength(p._2)

  def isMinimal(p: Path) = pathLength(p) == minimalLength

  def minimalLength = m max n
  def maximalLength = m + n

  /**
   * There's a bug in this method or in pathLessThan.
   * @param p
   * @return
   */
  def pathToSet(p: PartialPath): Set[Pos] = {

    def aux(positions: List[Pos], moves: List[Move]): List[Pos] = moves match {
      case hd :: tl => hd match {
        case Right => aux(positions.head.left :: positions, tl)
        case Up => aux(positions.head.down :: positions, tl)
        case Diagonally => aux(positions.head.diagonallyLeft :: positions, tl)
      }
      case List() => positions
    }

    aux(p._1 :: Nil, p._2).to(Set)
  }

  // Returns true if p1 viewed as a set is contained in p2.
  // There may be a bug in this method.
  def pathLessThan(p1: PartialPath, p2: PartialPath): Boolean =
    pathToSet(p1).map(pathToSet(p2).contains).reduce(_ && _) // Connection with my code?

  def pathToSet(p: Path): Set[Pos] = pathToSet((goal, p))

  // Returns true if p1 viewed as a set is contained in p2
  def pathLessThan(p1: Path, p2: Path): Boolean = pathLessThan((goal, p1), (goal, p2))
}
