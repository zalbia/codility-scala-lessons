package lessons._05.prefix_sums

object Examples {

  object counting_prefix_sums { // O(n)

    object procedural_style {
      def prefixSums(a: Array[Int]) = {
        val n          = a.length
        val prefixSums = Array.ofDim[Int](n + 1)
        for (k <- 1 to n)
          prefixSums(k) = prefixSums(k - 1) + a(k - 1)
        prefixSums
      }
    }

    object functional_style {
      def prefixSums(a: Array[Int]) =
        (1 to a.length).foldLeft(Vector.fill(a.length + 1)(0)) { (prefixSums, k) =>
          prefixSums.updated(k, prefixSums(k - 1) + a(k - 1))
        }
    }

    object scan {
      def prefixSums(a: Array[Int]) =
        (1 to a.length).scan(0)(_ + _)
    }
  }

  def prefixTotal(prefixSums: IndexedSeq[Int], p: Int, q: Int): Int =
    prefixSums(q + 1) - prefixSums(p)

  object mushroom_picker {

    object procedural_style {
      def mushroomPicker(mushroomSpots: Array[Int], start: Int, moves: Int): Int = {
        val n      = mushroomSpots.length
        var result = 0
        val pref   = mushroomSpots.scan(0)(_ + _)
        for (p <- 0 until math.min(moves, start)) {
          val leftPos  = start - p
          val rightPos = math.min(n - 1, math.max(start, start + moves - 2 * p))
          result = math.max(result, prefixTotal(pref, leftPos, rightPos))
        }
        for (p <- 0 until math.min(moves + 1, n - start)) {
          val rightPos = start + p
          val leftPos  = math.max(0, math.min(start, start - moves + 2 * p))
          result = math.max(result, prefixTotal(pref, leftPos, rightPos))
        }
        result
      }
    }

    object descriptive_names {
      def mushroomPicker(mushroomSpots: Array[Int], start: Int, moves: Int) = {
        val numSpots          = mushroomSpots.length
        var maxMushrooms      = 0
        val prefSums          = mushroomSpots.scan(0)(_ + _)
        // go left
        val farthestToTheLeft = math.min(moves, start)
        for (offset <- 0 until farthestToTheLeft) {
          val leftmostSpot      = start - offset
          val farthestAfterTurn = start + moves - 2 * offset
          val rightmostSpot     = math.min(numSpots - 1, math.max(start, farthestAfterTurn))
          maxMushrooms = math.max(maxMushrooms, prefixTotal(prefSums, leftmostSpot, rightmostSpot))
        }
        // go right
        val farthestToTheRight = math.min(moves + 1, numSpots - start)
        for (p      <- 0 until farthestToTheRight) {
          val rightmostSpot     = start + p
          val farthestAfterTurn = start - moves + 2 * p
          val leftmostSpot      = math.max(0, math.min(start, farthestAfterTurn))
          maxMushrooms = math.max(maxMushrooms, prefixTotal(prefSums, leftmostSpot, rightmostSpot))
        }
        maxMushrooms
      }
    }

    object refactor {

      object procedural_style {
        def mushroomPicker(mushroomSpots: Array[Int], start: Int, moves: Int) = {
          var maxMushrooms = 0
          val picker       = MushroomPicker(mushroomSpots, start, moves)
          for (totalPicked <- picker.pickLeftTotals)
            maxMushrooms = math.max(maxMushrooms, totalPicked)
          for (totalPicked <- picker.pickRightTotals)
            maxMushrooms = math.max(maxMushrooms, totalPicked)
          maxMushrooms
        }
      }

      object functional_style {
        def mushroomPicker(mushroomSpots: Array[Int], start: Int, moves: Int) = {
          val picker = MushroomPicker(mushroomSpots, start, moves)
          math.max(picker.pickLeftTotals.max, picker.pickRightTotals.max)
        }
      }

      case class MushroomPicker private (
        private val spots: Array[Int],
        start: Int,
        moves: Int
      ) {
        private val prefixSums = spots.scan(0)(_ + _)

        def pickLeftTotals: Iterator[Int] = {
          val farthestToTheLeft = math.min(moves, start)
          val goingLeft         = (0 until farthestToTheLeft).iterator
          goingLeft.map { offset =>
            val leftmostSpot      = start - offset
            val farthestAfterTurn = start + moves - 2 * offset
            val rightmostSpot     = math.min(spots.length - 1, math.max(start, farthestAfterTurn))
            prefixTotal(prefixSums, leftmostSpot, rightmostSpot)
          }
        }

        def pickRightTotals: Iterator[Int] = {
          val farthestToTheRight = math.min(moves + 1, spots.length - start)
          val goingRight         = (0 until farthestToTheRight).iterator
          goingRight.map { offset =>
            val rightmostSpot     = start + offset
            val farthestAfterTurn = start - moves + 2 * offset
            val leftmostSpot      = math.max(0, math.min(start, farthestAfterTurn))
            prefixTotal(prefixSums, leftmostSpot, rightmostSpot)
          }
        }
      }

      object MushroomPicker {
        def apply(spots: Array[Int], start: Int, moves: Int) =
          new MushroomPicker(spots, start, moves)
      }
    }
  }
}
