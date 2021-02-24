package lessons._13.fibonacci_numbers

import java.math.RoundingMode
import scala.annotation.tailrec

object FibonacciNumbers {

  object exponential_time { // O(2^n)
    def fibonacci(n: BigInt): BigInt =
      if (n <= 1) n else fibonacci(n - 1) + fibonacci(n - 2)
  }

  object dynamic_programming { // O(n)
    def fibonacci(n: Int): BigInt = {
      val fib = Array.fill(n.toInt + 2)(BigInt(0))
      fib(1) = 1
      for (i <- 2 to n + 1)
        fib(i) = fib(i - 1) + fib(i - 2)
      fib(n.toInt)
    }
  }

  object procedural_style { // O(n)
    def fibonacci(n: BigInt): BigInt = {
      if (n <= 1)
        return n
      var a = BigInt(0)
      var b = BigInt(1)
      for (_ <- BigInt(1) to n) {
        val next = a + b
        a = b
        b = next
      }
      a
    }
  }

  object tail_recursive { // O(n)
    def fibonacci(n: BigInt): BigInt = {
      @tailrec
      def acc(a: BigInt, b: BigInt, n: BigInt): BigInt =
        if (n == 0) a
        else if (n == 1) b
        else acc(b, a + b, n - 1)
      acc(0, 1, n)
    }
  }

  object fibonacci_formula {
    import java.math.{ BigDecimal => JBigDecimal, MathContext => JMathContext }

    // gives wrong answers past n = 163
    def fibonacci(n: Int): BigInt = {
      val sqrt5 = BigDecimal(JBigDecimal.valueOf(5L).sqrt(new JMathContext(1000, RoundingMode.HALF_UP)))
      val a     = ((1 + sqrt5) pow n) / (BigDecimal(2) pow n)
      val b     = ((1 - sqrt5) pow n) / (BigDecimal(2) pow n)
      ((a - b) / sqrt5).toBigInt
    }
  }

  object sum_of_two_fibonacci_numbers {

    def fibonacciSum(n: Int, max: Int): Set[(Int, Int)] = {
      val fibs = fibonacciNumsUpTo(max).toSet
      fibs
        .filter(m => fibs(n - m))
        .foldLeft(Set.empty[Int])((as, a) => if (as(a) || as(n - a)) as else as + a)
        .map(a => (a, n - a))
    }

    def fibonacciNumsUpTo(n: Int): Iterator[Int] = {
      if (n <= 1)
        return Iterator.single(n)

      new Iterator[Int] {
        var a                = 0
        var b                = 1
        def hasNext: Boolean = a <= n
        def next(): Int = {
          val next = a + b
          a = b
          b = next
          a
        }
      }
    }
  }
}
