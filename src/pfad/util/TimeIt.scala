package pfad.util

object TimeIt extends TimeIt

trait TimeIt {
  def timeIt(name: String, repeat: Int)(code: => Unit) {
    val start = System.currentTimeMillis
    1 to repeat foreach {_ => code}
    val end = System.currentTimeMillis
    val time = end - start
    val avg = time.toDouble / repeat
    println("Experiment: %s\n Total Time: %dms\n Average Time %fms" format (name, time, avg))
  }
}
