package lessons._06.sorting

object Sorting {

  object selection_sort { // O(n^2)
    import scala.Ordering.Implicits._

    object in_place {
      object procedural {
        // sorts copy in place
        def selectionSort[A: Ordering](a: Array[A]): Array[A] = {
          val sorted = a.slice(0, a.length)
          for (k <- sorted.indices) {
            var minimal = k
            for (j <- k + 1 until sorted.length)
              if (sorted(j) < sorted(minimal))
                minimal = j
            arraySwap(sorted, k, minimal)
          }
          sorted
        }
      }

      object higher_order_functions {
        def selectionSort[A: Ordering](a: Array[A]): Array[A] = {
          val sortedCopy = a.slice(0, a.length)
          for (k <- sortedCopy.indices)
            arraySwap(sortedCopy, k, (k until sortedCopy.length).minBy(sortedCopy))
          sortedCopy
        }
      }
    }

    // swaps in place
    def arraySwap[A](a: Array[A], p: Int, q: Int): Unit = {
      val temp = a(p)
      a(p) = a(q)
      a(q) = temp
    }

    object functional {
      def selectionSort[A: Ordering](a: Vector[A]): Vector[A] =
        a.indices.foldLeft(a) { (b, k) =>
          val indexMin = (k until b.length).minBy(b)
          b.updated(k, b(indexMin)).updated(indexMin, b(k))
        }
    }
  }

  object counting_sort extends scala.Integral.ExtraImplicits { // T: O(n + k) | S: O(k) ; k is # of distinct elements

    object procedural {
      def countingSort(a: Array[Int], k: Int): Array[Int] = {
        val sortCopy = a.slice(0, a.length)
        val counts   = Array.ofDim[Int](k + 1)
        for (i <- sortCopy.indices)
          counts(sortCopy(i)) += 1
        var cursor = 0
        for (i <- 0 to k) {
          for (_ <- 0 until counts(i)) {
            sortCopy(cursor) = i
            cursor += 1
          }
        }
        sortCopy
      }
    }

    // todo: is it worth doing a functional version?
  }
}
