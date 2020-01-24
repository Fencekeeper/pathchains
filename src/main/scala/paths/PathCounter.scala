package paths

object PathCounter extends App {
  val dim = 4

  val solverResults = {
    for {
      i <- (1 to dim).to(List)
      j <- (1 to dim).to(List)
      if (j <= i)
    } yield new Solver(i, j)
  }.
    map(s => s.dimensions + " " + s.pathsToGoal.length + " " + s.eulercharacteristic)

  solverResults.foreach(println)
}
