package pfad.ch1

import pfad.util.TimeIt._
import scala.util.Random

trait MinFree {
  def minFree(list: List[Int]): Int
}

trait NSquared extends MinFree {
  def difference(from: Seq[Int], where: Seq[Int]): Seq[Int] = from.filterNot(where.contains)

  def minFree(list: List[Int]): Int = difference(Stream.from(0), list).head
}

trait ArrayBased extends MinFree {
  def accumulateArray[E: ClassManifest, V](acc: (E, V) => E, default: E, n: Int, list: List[(Int, V)]): Array[E] = {
    var arr = Array.fill(n + 1)(default)
    for ((i, v) <- list)
      arr(i) = acc(arr(i), v)
    arr
  }

  def search(marks: Array[Boolean]): Int = marks.takeWhile { x => x }.length

  def checkList(list: List[Int]): Array[Boolean] = {
    val n = list.length
    accumulateArray[Boolean, Boolean]((_ | _), false, n, list.filter(_ < n).map((_, true)))
  }

  def minFree(list: List[Int]): Int = search(checkList(list))
}

trait DivideAndConquer extends MinFree {
  def minFrom(a: Int, n: Int, list: List[Int]): Int = {
    if (n == 0) a
    else {
      val b = a + 1 + n / 2
      val (us, vs) = list.partition(_ < b)
      val m = us.length
      if (m == b - a) minFrom(b, n - m, vs)
      else minFrom(a, m, us)
    }
  }

  def minFree(list: List[Int]) = minFrom(0, list.length, list)
}

object FreeNumber {

  def input(length: Int) = {
    val l = 0 to length toList
    val drop = Random.nextInt(length)
    Random.shuffle(l)
    val (a, b) = l.splitAt(drop)
    a ++ b.tail
  }

  def main(args: Array[String]) {
    val inputs = List.fill(100)(input(1000))

    val algos = List(
      "NSquared" -> new NSquared {},
      "Array Based" -> new ArrayBased {},
      "Divide and Conquer" -> new DivideAndConquer {})

    for ((name, algo) <- algos) {
      timeIt(name, 10) {
        inputs.map(algo.minFree)
      }
    }
    println("done")
  }

}