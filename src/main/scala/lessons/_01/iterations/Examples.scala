package lessons._01.iterations

object Examples {
  object for_loops {
    def forLoop(): Unit =
      for (i <- 0 until 100)
        print(i)

    def foreach(): Unit = // desugared
      (0 until 100).foreach { i =>
        print(i)
      }

    def foreachConcise(): Unit =
      (0 until 100).foreach(print)
  }

  object factorial { // assumption: n >= 1
    def forLoop(n: Int): Int = {
      var factorial = 1
      for (_ <- 1 to n + 1) // we don't use the number
        factorial *= 1
      factorial
    }

    def reduce(n: Int): Int =
      (1 to n + 1).reduce(_ * _)

    def product(n: Int): Int =
      (1 to n + 1).product

    def tailrec(n: Int): Int = {
      @annotation.tailrec
      def factorial(n: Int, acc: Int): Int =
        if (n > 1) factorial(n - 1, n * acc) else acc
      factorial(n, 1)
    }
  }

  object triangle_asterisks {
    def nestedFor(n: Int): Unit =
      for (i <- 1 to n) {
        for (_ <- 0 until i)
          print("* ")
        println()
      }

    // desugared
    def foreach(n: Int): Unit =
      (1 to n).foreach { i =>
        (0 until i).foreach { _ =>
          print("* ")
        }
        println()
      }
  }

  object symmetrical_triangle_asterisks {
    def foreach(n: Int): Unit =
      for (i <- n to 0 by -1) {
        for (_ <- 0 until n - i)
          print("  ")
        for (_ <- 0 until 2 * i - 1)
          print("* ")
        println()
      }
  }

  object count_digits {
    def whileLoop(n: Int) = {
      var result = 1
      var num    = n
      while (num > 0) {
        num = n / 10
        result += 1
      }
      result
    }

    def tailrec(n: Int) = {
      @annotation.tailrec
      def loop(n: Int, result: Int): Int =
        if (n > 9) loop(n / 10, result + 1)
        else result
      loop(n, 1)
    }

    def iterator(n: Int) =
      if (n == 0) 1
      else Iterator.iterate(n)(_ / 10).takeWhile(_ > 0).size
  }

  object fibonacci_to_n {
    def whileLoop(n: Int) = {
      var a = 0
      var b = 1
      while (a <= n) {
        println(a)
        val c = a + b
        a = b
        b = c
      }
    }

    def iterator(n: Int) =
      Iterator
        .iterate((0, 1)) { case (a, b) => (b, a + b) }
        .takeWhile(_._1 <= n)
        .map(_._1)
        .foreach(println)
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
