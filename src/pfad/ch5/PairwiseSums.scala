package pfad.ch5

import pfad.util.TimeIt._

trait SortedLists {

  def merge[T <% Ordered[T]](x: List[T], y: List[T]): List[T] = {
    import scala.util.control.TailCalls._
    def build(s: List[T], a: List[T], b: List[T]): TailRec[List[T]] = {
      if (a.isEmpty) {
        done(b.reverse ::: s)
      } else if (b.isEmpty) {
        done(a.reverse ::: s)
      } else if (a.head < b.head) {
        tailcall(build(a.head :: s, a.tail, b))
      } else {
        tailcall(build(b.head :: s, a, b.tail))
      }
    }
    build(Nil, x, y).result.reverse
  }

  type Label[A] = (A, (Int, Int))

  def subs[A](xs: List[A], ys: List[A])(implicit num: Numeric[A]) =
    for ((x, i) <- xs.zipWithIndex; (y, j) <- ys.zipWithIndex) yield (num.minus(x, y), (i, j))

  def sortsubs[A](xs: List[A], ys: List[A])(implicit num: Numeric[A]): List[Label[A]]

  def sortsums[A](xs: List[A], ys: List[A])(implicit num: Numeric[A]) = sortsubs(xs, ys.map(num.negate)).map(_._1)

}

trait Straightforward extends SortedLists {

  def sortsubs[A](xs: List[A], ys: List[A])(implicit num: Numeric[A]) = subs(xs, ys).sorted

}

// waay slower and glitchy :(
trait Lambert extends SortedLists {

  import collection.immutable.HashMap

  def table[A](xs: List[A], ys: List[A])(implicit num: Numeric[A]): List[(Int, Int, Int)] = {
    def tag(i: Int, xjk: Label[A]) = xjk match { case (x, (j, k)) => (x, (i, j, k)) }
    val xxs = sortsubs1(xs)
    val yys = sortsubs1(ys)
    merge(xxs.map(tag(1, _)), yys.map(tag(2, _))).map(_._2)
  }

  def mkArray[A](xs: List[A], ys: List[A])(implicit num: Numeric[A]) =
    table(xs, ys).zipWithIndex.toMap

  def cmp[A](a: Map[(Int, Int, Int), Int])(xij: Label[A], ykl: Label[A]) = (xij, ykl) match {
    case ((_, (i, j)), (_, (k, l))) =>
      a((1, i, k)) < a((2, j, l))
  }

  def sortsubs1[A](ws: List[A])(implicit num: Numeric[A]): List[Label[A]] =
    ws match {
      case Nil => Nil
      case w :: Nil => List((num.zero, (0, 0)))
      case ws =>
        val m = ws.size / 2
        val (xs, ys) = ws.splitAt(m)
        val xxs = sortsubs1(xs)
        val xys = subs(xs, ys).sortWith(cmp(mkArray(xs, ys)))
        val yxs = xys.reverse.map { case (x, (i, j)) => (num.negate(x), (j, i)) }
        val yys = sortsubs1(ys)
        List(
          xxs,
          xys.map { case (x, (i, j)) => (x, (i, m + j)) },
          yxs.map { case (x, (i, j)) => (x, (m + i, j)) },
          yys.map { case (x, (i, j)) => (x, (m + i, m + j)) })
          .reduceRight { (x: List[Label[A]], y: List[Label[A]]) => merge(x, y) }
    }

  def sortsubs[A](xs: List[A], ys: List[A])(implicit num: Numeric[A]) = subs(xs, ys).sortWith(cmp(mkArray(xs, ys)))

}

object PairwiseSums {

  def main(args: Array[String]) {

/*    val k = 50
    import scala.util.Random
    val xs = List.fill(k)(Random.nextInt(2*k)).sorted
    val ys = List.fill(k)(Random.nextInt(2*k)).sorted */
    
    val xs = List(1,2,3,4,5,6,7,8,9)
    val ys = List(11,12,13,14,15,16,17,18,19)
    
    val algos = List(
      "Straightforward" -> new Straightforward {},
      "Lambert" -> new Lambert {})

    for ((name, algo) <- algos) {
      timeIt(name, 10) {
        algo.sortsums(xs, ys)
      }
    }
  }
}