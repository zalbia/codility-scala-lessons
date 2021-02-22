package lessons._13.fibonacci_numbers

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
    import java.math.{ BigDecimal => JBigDecimal }
    import java.math.{ MathContext => JMathContext }

    def fibonacci(n: Int): BigInt = {
      val sqrt5 = BigDecimal(JBigDecimal.valueOf(5L).sqrt(JMathContext.DECIMAL128))
      val a = ((1 + sqrt5) / 2) pow n
      val b = ((1 - sqrt5) / 2) pow n
      ((a - b) / sqrt5).toBigInt
    }
  }
}
