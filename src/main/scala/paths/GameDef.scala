package paths

/**
 * I have to totally rethink this. Maybe one could use a simple recursion to
 * find all paths if necessary. However, this may be unnecessary. Maybe
 * I can use the path that goes via (0, n) as startPos and the one
 * that goes via (m, 0) as goal. Then I could explore, but the issue is
 * that I would not like to exclude paths as a build pathchains. So maybe
 * my problem is too different from this game?
 *
 * We could consider steps like replacing a Diagonally with a Right and an Up
 * or vice versa.
 *
 * Can I build the paths and the path chains simultaneously?
 *
 * An idea. Maybe I could use this exercise to demonstrate property-based testing
 * at the Itera conference and/or at some testforum? I would then have several tests
 * that I can implement, like the counting of paths and the Euler characteristic.
 *
 * Actually, I think there is room for a demo of property-based testing. Because
 * I can test that Solver(m, n) yields the same things as Solver(n, m), I can construct
 * tests concerning minimal paths, and there are in fact several properties
 * that either involve a single code unit or a combination of code units.
 */

trait GameDef {
  case class Pos(row: Int, col: Int) {
    def deltaRow(d: Int) = copy(row = row + d)
    def deltaCol(d: Int) = copy(col = col + d)
    def deltaRC(d1: Int, d2: Int) = copy(row = row + d1, col = col + d2)

    def <=(that: Pos) = row <= that.row && col <= that.col
    def <(that: Pos) = this <= that && this != that

    def right = deltaCol(1)
    def left = deltaCol(-1)
    def up = deltaRow(1)
    def down = deltaRow(-1)
    def diagonallyRight = deltaRC(1, 1)
    def diagonallyLeft = deltaRC(-1, -1)

    def isLegal: Boolean = terrain(this)

    def neighbors: List[(Pos, Move)] =
      (right, Right) :: (up, Up) :: (diagonallyRight, Diagonally) :: Nil

    def legalNeighbors: List[(Pos, Move)] =
      neighbors.filter(pm => pm._1.isLegal)
  }

  type Terrain = Pos => Boolean

  def terrain: Terrain
  def startPos: Pos
  def goal: Pos

  def done(pos: Pos) = pos == goal

  sealed abstract class Move
  case object Right extends Move
  case object Up extends Move
  case object Diagonally extends Move

}
