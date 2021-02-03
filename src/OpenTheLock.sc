val list682 = List(6, 8, 2)
val list614 = List(6, 1, 4)
val list206 = List(2, 0, 6)
val list738 = List(7, 3, 8)
val list380 = List(3, 8, 0)

val cluePairs = list206.zipWithIndex.permutations.map(_.take(2)).toList
val pairs = List(0, 4, 2).zipWithIndex.combinations(2).toList

pairs
  .map { case List((x1, i1), (x2, i2)) =>
    val legalPairs = cluePairs.filter { case List((y1, j1), (y2, j2)) =>
      println(s"pairings: $x1$x2, $y1$y2")
      val firstEqual = x1 == y1 && i1 != j1
      println(s"firstEqual: $firstEqual")
      val secondEqual = x2 == y2 && i2 != j2
      println(s"secondEqual: $secondEqual")
      firstEqual && secondEqual
    }.map { case List((x, _), (y, _)) => x -> y }
    println(s"legal pairs: $legalPairs")
    legalPairs.contains((x1, x2))
  }

list206.zipWithIndex.permutations.map(_.take(2)).toList

def oneDigitRightButInWrongPlace(candidate: List[Int], clue: List[Int]) = {
  candidate.to(LazyList).zip(clue)
    .map { case (a, b) => (clue.toSet - b contains a) && a != b }
    .reduce(_ || _)
}

def twoDigitsRightButInWrongPlace(candidate: List[Int], clue: List[Int]) = {
  val cluePairs = clue.zipWithIndex.permutations.map(_.take(2))
  candidate.zipWithIndex.combinations(2)
    .map { case List((x1, i1), (x2, i2)) =>
      val legalPairs = cluePairs.filter { case List((y1, j1), (y2, j2)) =>
        (x1 == y1 && i1 != j1) && (x2 == y2 && i2 != j2)
      }.map { case List((x, _), (y, _)) => x -> y }
      legalPairs.contains((x1, x2))
    }
    .reduce(_ || _)
}

// assuming lock has digits 0-9
val allPossibleCombinations =
  (0 to 9).permutations.to(LazyList).map(_.take(3)).map(_.toList).distinct

allPossibleCombinations
  .filter { combination =>
    // 682: One digit is right and in its place
    combination.zip(list682).map { case (a, b) => a == b }.reduce(_ || _) &&
      // 614: One digit is right but in the wrong place
      oneDigitRightButInWrongPlace(combination, list614) &&
      // 206: Two digits are right but in the wrong place
      twoDigitsRightButInWrongPlace(combination, list206) &&
      // 738: All digits are wrong
      combination.forall(!list738.contains(_)) &&
      // 380: One digit is right but in the wrong place
      oneDigitRightButInWrongPlace(combination, list380)
  }.force
