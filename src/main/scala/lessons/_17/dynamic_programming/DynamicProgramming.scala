package lessons._17.dynamic_programming

object DynamicProgramming {

  object dynamic_coin_changing {
    object _2d_array {

      object procedural_style {
        def dynamicCoinChanging(c: Array[Int], k: Int) = {
          val n       = c.length
          val rows    = n + 1
          val columns = k + 1
          val dp      = Array.ofDim[Int](rows, columns)
          dp(0) = Array(0) ++ Array.fill(k)(Int.MaxValue)
          for (i <- 1 until rows) {
            for (j <- 0 until c(i - 1))
              dp(i)(j) = dp(i - 1)(j)
            for (j <- c(i - 1) until columns)
              dp(i)(j) = math.min(dp(i)(j - c(i - 1)) + 1, dp(i - 1)(j))
          }
          dp(n)
        }
      }

      object functional_style {
        def dynamicCoinChanging(c: Array[Int], k: Int) = {
          val n       = c.length
          val rows    = n + 1
          val columns = k + 1
          val dp      = Vector
            .fill(rows, columns)(0)
            .updated(0, 0 +: Vector.fill(k)(Int.MaxValue))
          (1 until rows)
            .foldLeft(dp) { (dp, i) =>
              val dpc = (0 until c(i - 1)).foldLeft(dp) { (dp, j) =>
                dp.updated(i, dp(i).updated(j, dp(i - 1)(j)))
              }
              (c(i - 1) until columns).foldLeft(dpc) { (dp, j) =>
                dp.updated(i, dp(i).updated(j, math.min(dp(i)(j - c(i - 1)) + 1, dp(i - 1)(j))))
              }
            }
            .last
        }
      }

    }

    object _1d_array {

      object procedural_style {
        def dynamicCoinChanging(c: Array[Int], k: Int) = {
          val n  = c.length
          val dp = Array(0) ++ Array.fill(k)(Int.MaxValue)
          for (i <- 1 until n + 1)
            for (j <- c(i - 1) until k + 1)
              dp(j) = math.min(dp(j - c(i - 1)) + 1, dp(j))
          dp
        }
      }

      object functional_style {
        def dynamicCoinChanging(c: Array[Int], k: Int) =
          (1 until c.length + 1).foldLeft(0 +: Vector.fill(k)(Int.MaxValue)) { (dp, i) =>
            (c(i - 1) until k + 1).foldLeft(dp) { (dp, j) =>
              dp.updated(j, math.min(dp(j - c(i - 1)) + 1, dp(j)))
            }
          }
      }
    }
  }

  object dynamic_frog_jump {

    object procedural_style {
      def frog(s: Array[Int], k: Int, q: Int = Int.MaxValue) = {
        val dp = 1 +: Array.ofDim[Int](k + 1)
        for {
          j <- 1 until k + 1
          i <- s.indices if s(i) <= j
        } dp(j) = (dp(j) + dp(j - s(i))) % q
        dp(k)
      }
    }

    object functional_style {
      def frog(s: Array[Int], k: Int, q: Int = Int.MaxValue) = {
        val start = 1 +: Vector.fill(k + 1)(0)
        val dp    = (1 until k + 1).foldLeft(start) { (dp, j) =>
          s.indices.filter(s(_) <= j).foldLeft(dp) { (dp, i) =>
            dp.updated(j, (dp(j) + dp(j - s(i))) % q)
          }
        }
        dp(k)
      }
    }
  }
}
