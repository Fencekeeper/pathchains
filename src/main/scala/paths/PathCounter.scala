package paths

object PathCounter extends App {

  val dim = 5

  /**
   * Recall that we're only interested in the case when m >= n.
   */
  val solverResults = {
    for {
      i <- (1 to dim).to(List)
      j <- (1 to dim).to(List)
      if (j <= i)
    } yield new Solver(i, j)
  }.
    map(s => "Dim: " + s.dimensions + ", #paths: " + s.pathsToGoal.length + ", Euler char.: " + s.eulercharacteristic)

  solverResults.foreach(println)
}
