@main def testCombination = {
  val a: List[Int] = List(0, 1, 2, 3, 4, 5, 6)
  a.combinations(2).foreach { case List(a,b) =>
    println(s"Combination: ${a}, ${b}")
  }
}