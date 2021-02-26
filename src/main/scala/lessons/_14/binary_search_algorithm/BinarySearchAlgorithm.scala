package lessons._14.binary_search_algorithm

import scala.annotation.tailrec

object BinarySearchAlgorithm {

  object binary_search { // T: O(log n) assuming sorted array | S: O(1)

    object procedural_style {

      /**
       * @param a sorted array of ints
       * @param x int to find
       * @return index of x, -1 if not found
       */
      def binarySearch(a: Array[Int], x: Int): Int = {
        var beg      = 0
        var end      = a.length - 1
        var indexOfX = -1
        while (beg <= end) {
          val mid = (beg + end) / 2
          if (a(mid) <= x) {
            beg = mid + 1
            indexOfX = mid
          } else
            end = mid - 1
        }
        if (a(indexOfX) == x) indexOfX else -1
      }
    }

    object tail_recursive {
      def binarySearch(a: Array[Int], x: Int) = {
        @tailrec
        def loop(beg: Int = 0, end: Int = a.length - 1, i: Int = -1): Int =
          if (beg <= end) {
            val mid = (beg + end) / 2
            if (a(mid) <= x) loop(mid + 1, end, mid)
            else loop(beg, mid - 1, i)
          } else i
        val i                                                             = loop()
        if (a(i) == x) i else -1
      }
    }

    object functional_iterator {
      import Iterator.iterate

      def binarySearch(a: Array[Int], x: Int) = {
        val start  = (0, a.length - 1, -1)
        val search = iterate(start) { case (beg, end, i) =>
          val mid = (beg + end) / 2
          if (a(mid) <= x) (mid + 1, end, mid)
          else (beg, mid - 1, i)
        }

        val last      = search.find { case (beg, end, _) => beg > end }
        val found     = last.filter { case (_, _, result) => a(result) == x }
        val (_, _, i) = found.getOrElse(start)
        i
      }
    }
  }

  object holes_in_a_roof {

    object procedural_style {

      /**
       * @param a Holes in the roof
       * @param k number of boards
       *
       * @return size of the board, `-1` if you can't fill holes with `k` boards
       */
      def boards(a: Array[Int], k: Int): Int = {
        val n    = a.length
        var beg  = 1
        var end  = n
        var size = -1
        while (beg <= end) {
          val mid = (beg + end) / 2
          if (check(a, mid) <= k) {
            end = mid - 1
            size = mid
          } else
            beg = mid + 1
        }
        size
      }
    }

    object tail_recursive {
      def boards(a: Array[Int], k: Int): Int = {
        @tailrec
        def loop(beg: Int = 1, end: Int = a.length, size: Int = -1): Int =
          if (beg <= end) {
            val mid = (beg + end) / 2
            if (check(a, mid) <= k) loop(beg, end = mid - 1, size = mid)
            else loop(beg = mid + 1, end, size)
          } else size
        loop()
      }
    }

    object functional_iterator {
      import Iterator.iterate

      def boards(a: Array[Int], k: Int): Int = {
        val start  = (1, a.length - 1, -1)
        val search = iterate(start) { case (beg, end, size) =>
          val mid = (beg + end) / 2
          if (check(a, mid) <= k) (beg, mid - 1, mid)
          else (mid + 1, end, size)
        }

        val last         = search.find { case (beg, end, _) => beg > end }
        val (_, _, size) = last.getOrElse(start)
        size
      }
    }

    def check(a: Array[Int], k: Int): Int = {
      var boards = 0
      var last   = -1
      for (i <- a.indices)
        if (a(i) == 1 && last < i) {
          boards += 1
          last = i + k - 1
        }
      boards
    }
  }
}
