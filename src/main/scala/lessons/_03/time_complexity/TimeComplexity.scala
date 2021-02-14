package lessons._03.time_complexity

object TimeComplexity {

  /**
   * <pre>
   * ~10^6^ : O(n log n)
   * ~10^4^ : O(n^2^)
   * ~500 : O(n^3^)
   * </pre>
   */
  object time_complexity {
    def constant(n: Int) = // O(1)
      n * n

    object logarithmic_time { // O(log n)

      object while_loop {
        def logarithmic(n: Int) = {
          var num    = n
          var result = 0
          while (num > 1) {
            num = n / 2
            result += 1
          }
          result
        }
      }

      object iterator {
        def logarithmic(n: Int) =
          Iterator.iterate(n)(_ / 2).takeWhile(_ > 1).size
      }

    }

    object linear_time { // O(n)

      object for_loop {
        def linear(n: Int, a: Array[Int]): Int = {
          for (i <- 0 until n)
            if (a(i) == 0)
              return 0
          1
        }
      }

      object higher_order_functions {
        def linear(n: Int, a: Array[Int]): Int =
          (0 until n).find(a(_) == 0).map(_ => 0).getOrElse(1)
      }

    }

    object quadratic_time { // O(n^2)

      object nested_for_loops {
        def quadratic(n: Int) = {
          var result = 0
          for (i <- 0 until n)
            for (_ <- 0 until i)
              result += 1
          result
        }
      }

      object for_comprehension {
        def quadratic(n: Int) = {
          var result = 0
          for {
            i <- 0 until n
            _ <- 0 until i
          } result += 1
          result
        }
      }

      object for_comprehension_with_yield {
        def quadratic(n: Int) =
          (for {
            i <- 0 until n
            _ <- 0 until i
          } yield ()).size // "()" can be anything
      }

      object flatmap {
        def quadratic(n: Int) =
          (0 until n).flatMap(0 until _).map(_ => ()).size
      }

    }

    object two_variable_linear_time { // O(n + m)

      object two_for_loops {
        def linear2(n: Int, m: Int) = {
          var result = 0
          for (i <- 0 until n)
            result += i
          for (j <- 0 until m)
            result += j
          result
        }
      }

      object higher_order_functions_pure {
        def linear2(n: Int, m: Int) = {
          val nSum  = (0 until n).fold(0)(_ + _)
          val nmSum = (0 until m).fold(nSum)(_ + _)
          nmSum
        }
      }

      object constant_time_sum_method { // O(1)
        def actuallyConstant(n: Int, m: Int) =
          (0 until n).sum + (0 until m).sum
      }
    }
  }

  object total_of_1_to_n {

    object slow_solution { // O(n^2)

      object nested_for_loops {
        def sumTo(n: Int) = {
          var result = 0
          for (i <- 0 until n)
            for (_ <- 0 to i)
              result += 1
          result
        }
      }

      object for_comprehension {
        def sumTo(n: Int) = {
          var result = 0
          for {
            i <- 0 until n
            _ <- 0 to i
          } result += 1
          result
        }
      }

      object for_comprehension_with_yield {
        def sumTo(n: Int) =
          (for {
            i <- 0 until n
            _ <- 0 to i
          } yield ()).size
      }

      object desugared_for_yield {
        def sumTo(n: Int) =
          (0 until n).flatMap(0 to _).map(_ => ()).size
      }
    }

    object fast_solution { // O(n)

      object for_loop {
        def sumTo(n: Int) = {
          var result = 0
          for (i <- 0 until n) {
            result += i + 1
          }
          result
        }
      }

      object fold {
        def sumTo(n: Int) =
          (0 until n).fold(0)(_ + _ + 1)
      }
    }

    object model_solution { // O(1)

      def sumTo(n: Int) =
        n * (n + 1) / 2
    }
  }
}
