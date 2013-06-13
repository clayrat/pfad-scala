package pfad.ch3

import pfad.util.TimeIt._

trait Invert {
  type Fn = (Int, Int) => Int
  type Result = List[(Int, Int)]
  def invert(f: Fn, z: Int): Result
}

trait BSearch {
  def bsearch(g: (Int) => Int, a: Int, b: Int, z: Int): Int =
    if (a + 1 == b)
      a
    else {
      val m = (a + b) / 2
      if (g(m) <= z)
        bsearch(g, m, b, z)
      else
        bsearch(g, a, m, z)
    }
}

trait Square extends Invert {
  def invert(f: Fn, z: Int): Result =
    (for (
      x <- 0 to z;
      y <- 0 to z if f(x, y) == z
    ) yield (x, y)) toList
}

trait HalfSquare extends Invert {
  def invert(f: Fn, z: Int): Result =
    (for (
      x <- 0 to (z - f(0, 0));
      y <- 0 to (z - x - f(0, 0)) if f(x, y) == z
    ) yield (x, y)) toList
}

trait SaddlebackSearch extends Invert with BSearch {

  def invert(f: Fn, z: Int): Result = {
    val m = bsearch(f(0, _), -1, z + 1, z)
    val n = bsearch(f(_, 0), -1, z + 1, z)

    def find(u: Int, v: Int, f: Fn, z: Int): List[(Int, Int)] = {
      if (u > n || v < 0) Nil
      else {
        val z0 = f(u, v)
        if (z0 < z) find(u + 1, v, f, z)
        else if (z0 == z) (u, v) :: find(u + 1, v - 1, f, z)
        else find(u, v - 1, f, z)
      }
    }

    find(0, m, f, z)
  }
}

trait BinarySearch extends Invert with BSearch {

  def find(u: Int, v: Int, r: Int, s: Int, f: Fn, z: Int): Result = {
    if (u > r || v < s) Nil
    else {
      val p = (u + r) / 2
      val q = (v + s) / 2

      def rfind(p: Int): Result = {
        (if (f(p, q) == z) (p, q) :: find(u, v, p - 1, q + 1, f, z)
        else find(u, v, p, q + 1, f, z)) ++ find(p + 1, q - 1, r, s, f, z)
      }

      def cfind(q: Int): Result = {
        find(u, v, p - 1, q + 1, f, z) ++
          (if (f(p, q) == z) (p, q) :: find(p + 1, q - 1, r, s, f, z)
          else find(p + 1, q, r, s, f, z))
      }

      if (v - s <= r - u)
        rfind(bsearch(f(_, q), u - 1, r + 1, z))
      else
        cfind(bsearch(f(p, _), s - 1, v + 1, z))
    }
  }

  def invert(f: Fn, z: Int): Result = {
    val m = bsearch(f(0, _), -1, z + 1, z)
    val n = bsearch(f(_, 0), -1, z + 1, z)
    find(0, m, n, 0, f, z)
  }

}

object Saddleback {
  def main(args: Array[String]) = {

    def f3(x: Int, y: Int): Int = x + math.pow(2, y).toInt + y - 1
    val fn = f3 _

    val algos = List(
      "Square" -> new Square {},
      "Half Square" -> new HalfSquare {},
      "Saddleback Search" -> new SaddlebackSearch {},
      "Binary Search" -> new BinarySearch {})

    for ((name, algo) <- algos) {
      timeIt(name, 10) {
        algo.invert(fn, 50)
      }
    }

    println("done")
  }

}