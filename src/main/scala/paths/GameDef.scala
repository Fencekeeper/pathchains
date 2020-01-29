package paths

/**
 * An idea. Maybe I could use this exercise to demonstrate property-based testing
 * at the Itera conference and/or at some testforum? What would I have to do?
 * 1) Import ScalaTest and ScalaCheck - done
 * 2) Make a generator of paths (and partial paths?) - not done
 * 3) Write tests where there are at least several property-based tests (both unit and integration) - not done
 * 4) Try to make plausible errors so that a suite of property-based tests seems to be needed
 * and it is not the case that all errors are discovered by one or two simple tests. On the other hand,
 * ScalaCheck can save much work even with a few simple property-based tests, because these would replace
 * a great many non-property based tests. The reason is that although you can reason about boundary-values
 * then what if boundaries change? Property-based tests would likely accomodate such change.
 *
 * To check if this is viable as a demo, I need to come up with a reasonable number of properties
 * that I could define. At least some of them need to be interesting combinations of at least
 * two functions so that I demonstrate not only easier unit tests, but also the integration of
 * code units.
 *
 * 1) Count paths based on formula in master thesis
 * 2) Check that the 'chains' method includes at least the inputs
 * 3) Check that the chainsByLength method combined with chains method yields 0-chains that correspond
 * to the sum of the lengths of the two inputs to 'chains' method.
 * 4) Check symmetry of Grid(m, n) and Grid(n, m) and also for Solver(m, n) and Solver(n, m). E. g.
 * the counting of paths.
 * 5) check that function 'paths' includes the goal
 * 6) Check that the partial paths returned by neighborsWithHistory are of lengths +1
 * 7) Check that the number of legal neighbors is less than or equal to the number of neighbors
 * 8) Check that going left and then right leaves you at the outset. Similarly for up/down and diagonallyLeft/Right
 * 9) Some test concerning the terrain?
 * 10) Check that the function paths returns partial paths that have lengths between minimum and maximum
 * 11) Surely, I can construct tests concerning the comparison of two paths and concerning conversion to sets.
 * Probably 2-4 tests on this topic.
 *
 * All in all, I think this could actually be an ok demo. Because I can make a couple of drawings to illustrate
 * what we are actually doing and I can demonstrate not only unit tests, but also the integration
 * of code units. Now, specify the most interesting integration tests and then move on to create
 * a generator of paths.
 *
 */


trait GameDef {
  case class Pos(row: Int, col: Int) {
    private def deltaRow(d: Int) = copy(row = row + d)
    private def deltaCol(d: Int) = copy(col = col + d)
    private def deltaRC(d1: Int, d2: Int) = copy(row = row + d1, col = col + d2)

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
