import CharMatrixOps.SqueezedMatrixView
import CharMatrixOps.Style
import CharMatrixOps.replaceList
import scala.collection.mutable

class GitGraphReplacer(var lines: Array[Array[Char]], val style: Style) {
  var inner_paint: ((Int, Int), (Int, Int)) = null
  var max_column = -1
  var needle: SqueezedMatrixView = null
  var expected_size: (Int, Int) = null
  var substitutions: Map[SqueezedMatrixView,(Array[((Int, Int), Char)], ((Int, Int), (Int, Int)))] = Map()

  //def GitLines(lines: Vector[Vector[Char]]) {
     // this.lines = lines
      //this._paint = null
      //this.substitutions = {}
    //  this.max_column = -1
  //}

  def replace(args: String*) = {
      val size = (args.size, args(0).size)
      if (this.expected_size != null) {
        if (size != expected_size) {
          throw new RuntimeException("Wrong expected size")
        }
      } else {
        this.expected_size = (args.size, args(0).size)
      }
      this.needle = args.toArray.view.flatMap(_.toArray.view)
      this
  }

  def by(args: String*): Unit = {
      val size = (args.size, args(0).size)
      if (this.expected_size != null) {
        if (size != expected_size) {
          throw new RuntimeException("Wrong expected size")
        }
      } else {
        this.expected_size = (args.size, args(0).size)
      }

      val expected = this.needle

      val replacement = args.toArray.view.map(_.toArray.view)
      val replacementPoints = mutable.ArrayBuffer[((Int, Int), Char)]()
      //tuple([tuple(list(line)) for line in this.needle])


      for (i <- 0 until expected_size._1) {
        for (j <- 0 until expected_size._2) {
          if (needle(i*expected_size._2 + j) != replacement(i)(j)) {
            replacementPoints.append(((i, j), replacement(i)(j)))
          }
        }

      }

      this.substitutions = this.substitutions.updated(
          expected,
          (replacementPoints.toArray, this.inner_paint))
  }

  def set_maxcolumn(max_column: Int): Unit = {
      this.max_column = max_column
  }

  def run(): Unit = {
      replaceList(this.lines, this.substitutions, this.expected_size, style, this.max_column)
      this.inner_paint = null
      this.needle = null
      this.substitutions = Map()
      this.max_column = -1
      this.expected_size = null
  }

  def paint(args: String*) = {
      if (args.size == 0) {
          this.inner_paint = null
      } else {
          var p = List.empty[(Int, Int)]
          for (i <- args.indices) {
              for (j <- args(i).indices) {
                  if (args(i)(j) == 'S') {
                      p = p :+ (i, j)
                  }
              }
          }

          for (i <- args.indices) {
              for (j <- args(i).indices) {
                  if (args(i)(j) == 'D') {
                      p = p :+ (i, j)
                  }
              }
          }

          this.inner_paint = (p(0), p(1))
      }
  }
}