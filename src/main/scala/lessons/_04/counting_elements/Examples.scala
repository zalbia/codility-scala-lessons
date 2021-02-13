package lessons._04.counting_elements

object Examples {

  object counting_elements {

    object procedural {
      def counting(a: Array[Int], m: Int) = { // count
        val counts = Array.fill(m + 1)(0)
        for (k <- a.indices)
          counts(a(k)) += 1
        counts
      }
    }

    object functional {
      def counting(a: Array[Int], m: Int): Vector[Int] =
        a.indices.foldLeft(Vector.fill(m + 1)(0)) { (counts, k) =>
          counts.updated(a(k), counts(a(k)) + 1)
        }
    }
  }

  object swap_equilibrium {

    object slow_solution { // O(n^2)

      object procedural_style {
        def swapEquilibrium(a: Array[Int], b: Array[Int], m: Int): Boolean = {
          val n    = a.length
          var sumA = a.sum
          var sumB = b.sum
          for {
            i <- 0 until n
            j <- 0 until n
          } {
            val change = b(j) - a(i)
            sumA += change
            sumB -= change
            if (sumA == sumB)
              return true
            sumA -= change
            sumB += change
          }
          false
        }
      }

      object functional_style {
        def swapEquilibrium(a: Array[Int], b: Array[Int], m: Int): Boolean = {
          val n          = a.length
          val sumA       = a.sum
          val sumB       = b.sum
          val swapEquals = for {
            i <- 0 until n
            j <- 0 until n
          } yield {
            val change = b(j) - a(i)
            sumA + change == sumB - change
          }
          swapEquals.exists(identity)
        }
      }
    }

    object fast_solution {
      import counting_elements.procedural.counting

      object procedural_style {

        def swapEquilibrium(a: Array[Int], b: Array[Int], m: Int): Boolean = {
          val sumDiff = a.sum - b.sum
          if (sumDiff % 2 == 1)
            return false
          val d     = sumDiff / 2
          val count = counting(a, m)
          for (i <- a.indices)
            if (0 <= b(i) - d && b(i) - d <= m && count(b(i) - d) > 0)
              return true
          false
        }
      }

      object functional_style {

        def swapEquilibrium(a: Array[Int], b: Array[Int], m: Int): Boolean = {
          val sumDiff = a.sum - b.sum
          if (sumDiff % 2 == 1) false
          else {
            val d     = sumDiff / 2 // because there are two arrays
            val count = counting(a, m)
            a.indices.exists { i =>
              val swapper   = b(i) - d
              val validSwap = 0 <= swapper && swapper <= m
              validSwap && count(swapper) > 0
            }
          }
        }
      }
    }
  }
}
