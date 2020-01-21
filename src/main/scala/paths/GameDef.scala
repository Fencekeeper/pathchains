package paths

trait GameDef {
  case class Pos(row: Int, col: Int) {
    def deltaRow = copy(row = row + 1)
    def deltaCol = copy(col = col + 1)
    def deltaRC = copy(row = row + 1, col = col + 1)
    def <=(that: Pos) = row <= that.row && col <= that.col
    def <(that: Pos) = this <= that && this != that
  }

  type Terrain = Pos => Boolean

  def terrain: Terrain
  def startPos: Pos
  def goal: Pos

  sealed abstract class Move
  case object Right extends Move
  case object Up extends Move
  case object Diagonally extends Move

  case class Block(pos: Pos) {
    def right = Block(pos.deltaCol)
    def up = Block(pos.deltaRow)
    def diagonally = Block(pos.deltaRC)

    def isLegal: Boolean = terrain(pos)

    def neighbors: List[(Block, Move)] =
      (right, Right) :: (up, Up) :: (diagonally, Diagonally) :: Nil

    def legalNeighbors: List[(Block, Move)] =
      neighbors.filter(bm => bm._1.isLegal)
  }
}
