package paths

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

  // For simplicity, a path is converted into a set.
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

  /**
   * This function is inefficient because the paths are converted to sets. One could
   * make an algorithm that compares paths without this conversion, although I suspect
   * readability would be worse.
   *
   * @param p1 is a PartialPath to be compared with p2
   * @param p2 is a PartialPath
   * @return true if p1 viewed as a set is included in p2 viewed as a set
   */
  def pathLessThan(p1: PartialPath, p2: PartialPath): Boolean =
    pathToSet(p1).map(pathToSet(p2).contains).reduce(_ && _) // Connection with my code?

  def pathToSet(p: Path): Set[Pos] = pathToSet((goal, p))

  // Returns true if p1 viewed as a set is contained in p2
  def pathLessThan(p1: Path, p2: Path): Boolean = pathLessThan((goal, p1), (goal, p2))
}
