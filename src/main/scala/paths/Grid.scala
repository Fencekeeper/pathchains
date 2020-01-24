package paths

/**
 * Recall that we're only interested in the case when m >= n.
 */
class Grid(m: Int, n: Int) extends GameDef {

  override def startPos = Pos(0, 0)
  override def goal: Pos = Pos(m, n)
  def dimensions = (m, n)

  override def terrain: Terrain =
    p => startPos <= p && p <= goal

  type PartialPath = (Block, List[Move])

  def pathLength(p: PartialPath) = p._2.length

  def pathToSet(p: PartialPath): Set[Pos] = {

    def aux(positions: List[Pos], moves: List[Move]): List[Pos] = moves match {
      case hd :: tl => hd match {
        case Right => positions.head.left :: positions
        case Up => positions.head.down :: positions
        case Diagonally => positions.head.diagonallyLeft :: positions
      }
      case Nil => positions
    }

    aux(p._1.pos :: Nil, p._2).to(Set)
  }

  // Returns true if p1 viewed as a set is contained in p2
  def pathLessThan(p1: PartialPath, p2: PartialPath): Boolean =
    pathToSet(p1).map(pathToSet(p2).contains).reduce(_ && _)
}
