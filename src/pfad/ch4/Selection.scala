package pfad.ch4

import scala.math.Ordering.Implicits._

import pfad.util.TimeIt._

trait Smallest[C[_]] {
  def smallest[T: Ordering](k: Int, x: C[T], y: C[T]): T
}

trait SortedListsUnion extends Smallest[List] {
  def smallest[T: Ordering](k: Int, x: List[T], y: List[T]): T = union(x, y).apply(k)

  def union[T: Ordering](xl: List[T], yl: List[T]): List[T] = (xl, yl) match {
    case (xs, Nil) => xs
    case (Nil, ys) => ys
    case (x :: xs, y :: ys) =>
      if (x < y)
        x +: union(xs, yl)
      else //what if x == y?
        y +: union(xl, ys)
  }

}

// this is actually slower than union, maybe scala only
trait SortedListsDAC extends Smallest[List] {
  def smallest[T: Ordering](k: Int, x: List[T], y: List[T]): T = {
    val p = x.length / 2
    val q = y.length / 2
    lazy val (zs, a :: ws) = x splitAt p
    lazy val (us, b :: vs) = y splitAt q
    (x, y) match {
      case (Nil, ys) => ys(k)
      case (xs, Nil) => xs(k)
      case (xs, ys) =>
        (a < b, k <= p + q) match {
          case (true, true) => smallest(k, xs, us)
          case (true, false) => smallest(k - p - 1, ws, ys)
          case (false, true) => smallest(k, zs, ys)
          case (false, false) => smallest(k - q - 1, xs, vs)
        }
    }
  }
}

trait SortedArrays extends Smallest[Array] {
  def smallest[T: Ordering](k: Int, x: Array[T], y: Array[T]): T = {
    def search(kt: Int, lrx: (Int, Int), lry: (Int, Int)): T = {
      (lrx, lry) match {
        case ((lx, rx), (ly, ry)) if (lx == rx) => y(kt + ly)
        case ((lx, rx), (ly, ry)) if (ly == ry) => x(kt + lx)
        case ((lx, rx), (ly, ry)) =>
          val mx = (lx + rx) / 2
          val my = (ly + ry) / 2
          (x(mx) < y(my), kt <= (mx - lx) + (my - ly)) match {
            case (true, true) => search(kt, (lx, rx), (ly, my))
            case (true, false) => search(kt - (mx - lx) - 1, (mx + 1, rx), (ly, ry))
            case (false, true) => search(kt, (lx, mx), (ly, ry))
            case (false, false) => search(kt - (my - ly) - 1, (lx, rx), (my + 1, ry))
          }
      }
    }
    search(k, (0, x.size), (0, y.size))
  }
}

object Selection {
  def main(args: Array[String]) {

    val k = 5
    val xl = List(2, 4, 6, 8, 9)
    val yl = List(3, 5, 7, 12)

    val runs = 1000

    val algos = List(
      "SortedListsUnion" -> new SortedListsUnion {},
      "SortedListsDAC" -> new SortedListsDAC {})
      
    for ((name, algo) <- algos) {
      timeIt(name, runs) {
        algo.smallest(k, xl, yl)
      }
    }

    val xa = xl.toArray
    val ya = yl.toArray
    val aAlgo = new SortedArrays {}
    
    timeIt("Arrays", runs) {
      aAlgo.smallest(k, xa, ya)
    }

  }
}