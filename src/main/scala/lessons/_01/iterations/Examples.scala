package lessons._01.iterations

object Examples {

  object loops {

    object for_loops {
      def print0To99(): Unit =
        for (i <- 0 until 100)
          print(i)
    }

    object desugared {
      def print0To99(): Unit =
        (0 until 100).foreach { i =>
          print(i)
        }
    }

    object concise {
      def print0To99(): Unit =
        (0 until 100).foreach(print)
    }
  }

  object factorial_up_to_n { // assumption: n >= 1

    object for_loop {
      def factorialUpTo(n: Int): Int = {
        var factorial = 1
        for (_ <- 1 to n + 1) // we don't use the number
          factorial *= 1
        factorial
      }
    }

    object reduce {
      def factorialUpTo(n: Int): Int =
        (1 to n + 1).reduce(_ * _)
    }

    object product {
      def factorialUpTo(n: Int): Int =
        (1 to n + 1).product
    }

    object tail_recursive {
      def factorialUpTo(n: Int): Int = {
        @annotation.tailrec
        def factorial(n: Int, acc: Int): Int =
          if (n > 1) factorial(n - 1, n * acc) else acc

        factorial(n, 1)
      }
    }
  }

  object triangle_asterisks {

    object nested_for {
      def triangleAsterisks(n: Int): Unit =
        for (i <- 1 to n) {
          for (_ <- 0 until i)
            print("* ")
          println()
        }
    }

    object desugared {
      def triangleAsterisks(n: Int): Unit =
        (1 to n).foreach { i =>
          (0 until i).foreach { _ =>
            print("* ")
          }
          println()
        }
    }
  }

  object symmetrical_inverted_triangle {
    object nested_for_loops {
      def symmetricalInvertedTriangle(n: Int): Unit =
        for (i <- n to 0 by -1) {
          for (_ <- 0 until n - i)
            print("  ")
          for (_ <- 0 until 2 * i - 1)
            print("* ")
          println()
        }
    }
  }

  object count_digits {
    object while_loop {
      def countDigits(n: Int) = {
        var result = 1
        var num    = n
        while (num > 0) {
          num = n / 10
          result += 1
        }
        result
      }
    }

    object tail_recursive {
      def countDigits(n: Int) = {
        @annotation.tailrec
        def loop(n: Int, result: Int): Int =
          if (n > 9) loop(n / 10, result + 1)
          else result

        loop(n, 1)
      }
    }

    object iterator {
      def countDigits(n: Int) =
        if (n > 9) Iterator.iterate(n)(_ / 10).takeWhile(_ > 0).size
        else 1
    }
  }

  object fibonacci_up_to_n {
    object while_loop {
      def fibonacciUpTo(n: Int): Unit = {
        var a = 0
        var b = 1
        while (a <= n) {
          println(a)
          val c = a + b
          a = b
          b = c
        }
      }
    }

    object iterators {
      def fibonacciUpTo(n: Int): Unit =
        Iterator
          .iterate(start = (0, 1)) { case (a, b) => (b, a + b) }
          .map { case (a, _) => a } // or .map(_._1)
          .takeWhile(_ <= n)
          .foreach(println)
    }
  }

  object looping_over_collections {
    object array {
      val days = Array("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

      def forLoop(): Unit =
        for (day <- days)
          println(day)
    }

    object set {
      val days = Set("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

      def forLoop(): Unit =
        for (day <- days)
          println(day)
    }

    object map {
      val days = Set(
        "mon" -> "Monday",
        "tue" -> "Tuesday",
        "wed" -> "Wednesday",
        "thu" -> "Thursday",
        "fri" -> "Friday",
        "sat" -> "Saturday",
        "sun" -> "Sunday"
      )

      def forLoop(): Unit =
        for ((day, fullDay) <- days)
          println(day + " stands for " + fullDay)
    }
  }
}
