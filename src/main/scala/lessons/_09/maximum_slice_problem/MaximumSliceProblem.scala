package lessons._09.maximum_slice_problem

object MaximumSliceProblem {

  object maximal_slice {

    object cubic_time { // O(n^3)

      object procedural_style {
        def cubicMaxSlice[A: Numeric](a: Array[A]): A = {
          val numeric = implicitly[Numeric[A]]
          var result  = numeric.zero
          for (p <- a.indices)
            for (q <- p until a.length)
              result = numeric.max(result, a.iterator.slice(p, q + 1).sum)
          result
        }
      }

      object functional_style {
        def cubicMaxSlice[A: Numeric](a: Array[A]): A = {
          val sums =
            for {
              p <- a.indices.iterator
              q <- (p until a.length).iterator
            } yield a.iterator.slice(p, q + 1).sum
          sums.max
        }
      }
    }

    object quadratic_time { // O(n^2)

      object with_prefix_sums {
        object procedural_style {
          def quadraticMaxSlice[A: Numeric](a: Array[A]): A = {
            val numeric = implicitly[Numeric[A]]
            val pref    = a.scan(numeric.zero)(numeric.plus)
            var result  = numeric.zero
            for (p <- a.indices)
              for (q <- p until a.length) {
                val sum = numeric.minus(pref(q + 1), pref(p))
                result = numeric.max(result, sum)
              }
            result
          }
        }

        object functional_style {
          def quadraticMaxSlice[A: Numeric](a: Array[A]): A = {
            val numeric = implicitly[Numeric[A]]
            val pref    = a.scan(numeric.zero)(numeric.plus)
            val sums    =
              for {
                p <- a.indices.iterator
                q <- (p until a.length).iterator
              } yield numeric.minus(pref(q + 1), pref(p))
            sums.max
          }
        }
      }

      object without_prefix_sums {

        object procedural_style {
          def quadraticMaxSlice[A: Numeric](a: Array[A]): A = {
            val numeric = implicitly[Numeric[A]]
            var result  = numeric.zero
            for (p <- a.indices) {
              var sum = numeric.zero
              for (q <- p until a.length) {
                sum = numeric.plus(sum, a(q))
                result = numeric.max(result, sum)
              }
            }
            result
          }
        }

        object functional_style {
          def quadraticMaxSlice[A: Numeric](a: Array[A]): A = {
            val numeric = implicitly[Numeric[A]]
            a.indices.iterator.foldLeft(numeric.zero) { (maxAcc, p) =>
              val (newMax, _) = (p until a.length).iterator.foldLeft((maxAcc, numeric.zero)) {
                case ((result, sum), q) =>
                  val newSum = numeric.plus(sum, a(q))
                  (numeric.max(result, newSum), newSum)
              }
              newMax
            }
          }
        }
      }
    }

    object linear_time { // O(n)

      object procedural_style {
        def goldenMaxSlice[A: Numeric](a: Array[A]): A = {
          val numeric             = implicitly[Numeric[A]]
          var maxEnding, maxSlice = numeric.zero
          for (elem <- a) {
            maxEnding = numeric.max(numeric.zero, numeric.plus(maxEnding, elem))
            maxSlice = numeric.max(maxSlice, maxEnding)
          }
          maxSlice
        }
      }

      object functional_style {
        def goldenMaxSlice[A: Numeric](a: Array[A]): A = {
          val numeric       = implicitly[Numeric[A]]
          val (_, maxSlice) = a.foldLeft((numeric.zero, numeric.zero)) { case ((maxEnding, maxSlice), elem) =>
            val newMaxEnding = numeric.max(numeric.zero, numeric.plus(maxEnding, elem))
            val newMaxSlice  = numeric.max(maxSlice, newMaxEnding)
            (newMaxEnding, newMaxSlice)
          }
          maxSlice
        }
      }
    }
  }
}
