package pfad.ch2

import pfad.util.TimeIt._
import scala.util.Random

trait MaximumSurpasserCount {
  def msc[T: Ordering](list: List[T]): Int
}

trait Quadratic extends MaximumSurpasserCount {
  
  def tails[T](list: List[T]): List[List[T]] =
    list match {
      case Nil => Nil
      case x :: xs => list :: tails(xs)
    }

  def scount[T: Ordering](x: T, xs: List[T]): Int = xs.filter { y => implicitly[Ordering[T]].lt(x, y) }.length

  def msc[T: Ordering](list: List[T]): Int = (for (x :: xs <- tails(list)) yield scount(x, xs)).max
}

trait DivideAndConquer extends MaximumSurpasserCount {
  type Table[T] = List[(T, Int)]

  def lt[T: Ordering](a: T, b: T) = implicitly[Ordering[T]].lt(a, b)
  def lteq[T: Ordering](a: T, b: T) = implicitly[Ordering[T]].lteq(a, b)

  def tcount[T: Ordering](z: T, t: Table[T]): Int =
    t.dropWhile { x => lteq(x._1, z) }.length

  def scount[T: Ordering](x: T, xs: List[T]): Int =
    xs.filter { y => lt(x, y) }.length

  def table[T: Ordering](list: List[T]): Table[T] = list match {
    case List(x) => List((x, 0))
    case list => {
      val m = list.length
      val n = m / 2
      val (xs, ys) = list.splitAt(n)
      join(m - n, table(xs), table(ys))
    }
  }

  def join[T: Ordering](n: Int, txs: Table[T], tys: Table[T]): Table[T] = (txs, tys) match {
    case (txs, Nil) => txs
    case (Nil, tys) => tys
    case ((x, c) :: xss, (y, d) :: yss) =>
      if (lt(x, y))
        (x, c + n) :: join(n, xss, tys)
      else (y, d) :: join(n - 1, txs, yss)
  }

  def msc[T: Ordering](list: List[T]): Int =
    table(list).map(_._2).max

}

object Surpassing {

  def input(length: Int, maxVal: Int): List[Int] =
    List.fill(length + 1)(Random.nextInt(maxVal))

  def main(args: Array[String]) = {

    val inputs = List.fill(10)(input(2000, 1000))

    val algos = List(
      "Quadratic" -> new Quadratic {},
      "DivideAndConquer" -> new DivideAndConquer {})

    for ((name, algo) <- algos) {
      timeIt(name, 10) {
        inputs.map(algo.msc(_))
      }
    }
    println("done")
  }

}