package utils

object EasyMetrics {
  var t0 = -1L
  var mark_ = System.nanoTime()
  var t1 = System.nanoTime()
  var t2 = System.nanoTime()
  var timeElapsed = -1L

  def startMeasurament() = {
    timeElapsed = -1L
    if (t0 == -1L) {
      t0 = System.nanoTime();
    }
    t1 = System.nanoTime();
  }

  def mark() {
    mark_ = System.nanoTime()
  }

  def took() = {
    t2 = System.nanoTime();
    timeElapsed = (t2 - t1) / 1000000
    t1 = System.nanoTime();
    timeElapsed
  }

  def globalTook() = {
    t2 = System.nanoTime();
    timeElapsed = (t2 - t0) / 1000000
    timeElapsed
  }

  def tookFromMark() = {
    t2 = System.nanoTime();
    timeElapsed = (t2 - mark_) / 1000000
    timeElapsed
  }
}