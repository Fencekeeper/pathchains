package paths

object PathCounter extends App {

  /*
  val s = new Solver(1, 1)
  println("Paths to goal:")
  s.pathsToGoal.foreach(println)
  println("\nPaths grouped by minimality:")
  println(s.pathsGroupedByMinimality)
  println("\nPaths grouped by length:")
  println(s.pathsGroupedByLength)
  println("\nEuler Characteristic: " + s.eulercharacteristic)

   */

  ///*
  val dim = 3

  val solverResults = {
    for {
      i <- (1 to dim).to(List)
      j <- (1 to dim).to(List)
      if (j <= i)
    } yield new Solver(i, j)
  }.
    map(s => s.dimensions + " " + s.pathsToGoal.length + " " + s.eulercharacteristic)

  solverResults.foreach(println)
  //*/
}
