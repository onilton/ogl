import scala.io.Source
import util.control.Breaks._
import scala.collection.mutable
import EasyMetrics._


object ogl {
  def main(args: Array[String]): Unit = {
        println("Started")
        startMeasurament()
        //val data1 = Source.fromFile("um_tempcolor", "utf-8").getLines
        // Remember a line is broken(encoding)
        // better iterate in loop to avoid errors
        val data1 = Source.fromFile("um_tempcolor", "utf-8").getLines.toArray
        //#with open("um_tempcolor", 'r') as file1:
        //#    data1 = file1.read()

        //#with open("docs/fourthstyle", 'r') as file1:
        //#    data1 = file1.read()

// #result = subprocess.run([
// #    'git',
// #    'log',
// #    '--graph',
// #    #'--pretty="format:\\%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset"',
// #    #'--pretty=format:\%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset',
// #    '--pretty=format:%h -%d %s (%cr) <%an>',
// #    '--abbrev-commit',
// #    '--color'
// #    ],
// #    stdout=subprocess.PIPE)
// #data1 = result.stdout.decode("utf-8")

def parse_line(line: String): (Array[(String, String)], String) = {
    var escapes: mutable.ArrayBuffer[(String, String)] = mutable.ArrayBuffer()
    var escapes_so_far = ""
    var clean_line: StringBuilder =  new StringBuilder()

    var index = 0
    var clean_index = 0
    while (index < line.size) {
        if (line(index) == '\u001b') {
            val start = line.indexOf("\u001b[", index)
            if (start != 1) {
                val end = line.indexOf("m", start)
                if ((end - start) <= 2) {
                    //#print(line[start:end + 1])
                    //escapes(len(escapes)-1)(1) = line.slice(start,end + 1)
                    // escapes = escapes.updated(
                    //     len(escapes)-1,
                    //     (escapes(len(escapes) -1)._1, line.substring(start, end+1)))
                    escapes(escapes.size-1) =
                        (escapes(escapes.size -1)._1, line.substring(start, end+1))
                    //(len(escapes)-1)(1) = line.slice(start,end + 1)
                } else {
                    escapes_so_far += line.slice(start,end + 1)
                }
                index = end
            }
        } else {
            //escapes = escapes :+ (escapes_so_far, "")
            escapes.append((escapes_so_far, ""))
            escapes_so_far = ""
            clean_line.append(line(index))
            clean_index += 1
        }
        index = index + 1
    }
    return (escapes.toArray, clean_line.toString())
}


val first_non_graph_rgx = """[^*|\\/ _]""".r
val allGraphChars = Set('*','|','\\','/',' ','_')

type CharMatrix = Array[Array[Char]]

type CharMatrixView = scala.collection.SeqView[scala.collection.mutable.IndexedSeqView[Char,Array[Char]],Array[scala.collection.mutable.IndexedSeqView[Char,Array[Char]]]]
type SqueezedMatrixView = scala.collection.SeqView[Char,Array[Char]]


println("File load " + took())

  var style: mutable.ArrayBuffer[Array[(String, String)]] =
      new mutable.ArrayBuffer[Array[(String, String)]](data1.size)
  var graph_lines: mutable.ArrayBuffer[Array[Char]] = new mutable.ArrayBuffer[Array[Char]](data1.size)
  var messages: mutable.ArrayBuffer[String] = new mutable.ArrayBuffer[String](data1.size)

  var max_graph_idx = -1

  for (raw_line <- data1) {
      val (escapes, line) = parse_line(raw_line)

      val index = line.indexWhere(c => !allGraphChars.contains(c))

      val (graph, message) =
        if (index >= 0) {
          val last_graph_idx = index
          line.splitAt(last_graph_idx)
        } else {
          (line, "")
        }

      if (graph.exists(c => c == '/' || c == '\\')) {
          var extended = graph.replace("*", "|")

          extended = extended.replace("_", " ")
          style.append(escapes)
          graph_lines.append(extended.toArray)
          messages.append("")
      }

      graph_lines.append(graph.replace("|_", "|─").toArray)
      style.append(escapes.toArray)
      messages.append(message)
  }

  print("Summary | max graph idx: " + max_graph_idx)
  print(" | lines: " + graph_lines.size)
  println(" | original lines: " + data1.size)
  println("Splitted graph " + took())
  startMeasurament()


  def get_sub_matrix_view(target: Array[Array[Char]],
                          start_pos: (Int, Int),
                          size: (Int, Int)): SqueezedMatrixView = {
    val (start_x, start_y) = start_pos
    val (height, width) = size
    val end_y = start_y + width

    val target_height = target.size

    var tempArray = Array.ofDim[Char](height*width)
    var view = tempArray.view

    var i = 0
    var j = 0
    var column = 0
    while (i < height) {
        j = 0
        while (j < width) {
            tempArray(column +  j) = target(start_x + i)(start_y + j)
            j+=1
        }
        column += j
        i+=1
    }

    return view
  }

  def replace_matrix(replacement: CharMatrixView,
                    target: Array[Array[Char]],
                    start_pos: (Int, Int)) = {
    val (start_x, start_y) = start_pos

    var i = 0
    var j = 0
    while (i < replacement.size) {
      j = 0
      while (j < replacement(i).size) {
        target(start_x + i)(start_y + j) = replacement(i)(j)
        j+=1
      }
      i+=1
    }

    target
  }

  def getSmartSet(keySet: Set[SqueezedMatrixView], size: (Int, Int)) = {
    val smartSet = Array.fill[Set[Char]](size._1, size._2)(Set())
    for (i <- 0 until size._1) {
      for (j <- 0 until size._2) {
        smartSet(i)(j) = keySet.map(m => m(i*size._2 + j))
      }
    }

    smartSet
  }

  def replace_list_2x1(target: Array[Array[Char]],
                      substitutions: Map[SqueezedMatrixView,(Array[((Int, Int), Char)], ((Int, Int), (Int, Int)))],
                      max_column: Int = -1) {
    val expected_size = (2, 1)

    val smartSet = getSmartSet(substitutions.keySet, expected_size)

    var lidx = 0
    var ridx = 0
    while (lidx < (target.size - 1)) {
      val max_possible_col_idx = target(lidx).size
      val inner_max_column = if (max_column != -1 && max_column < max_possible_col_idx ) {
        max_column + 1
      } else {
        max_possible_col_idx
      }

      ridx = 0
      while (ridx < inner_max_column) {
        var found: (Array[((Int, Int), Char)], ((Int,Int), (Int, Int))) = null

        val nextLineIsValid = ridx < target(lidx+1).size

        if (nextLineIsValid) {
          val item00 = target(lidx)(ridx)
          val item10 = target(lidx+1)(ridx)

          if (smartSet(0)(0).contains(item00) &&
              smartSet(1)(0).contains(item10)) {
            val tempArray = Array.ofDim[Char](2)
            tempArray(0) = item00
            tempArray(1) = item10

            found = substitutions.getOrElse(tempArray.view, null)
          }
        }

        while (found != null) {
          val pair = found

          val replacement = pair._1

          var paint = pair._2
          var k = 0
          while (k < replacement.size) {
            val ((x, y), c) = replacement(k)
            target(lidx + x)(ridx + y) = c
            k += 1
          }

          if (paint != null) {
              val (source, dest) = paint
              val source_line = lidx + source._1
              val source_column = ridx + source._2
              val dest_line = lidx + dest._1
              val dest_column = ridx + dest._2
              val source_style = style(source_line)(source_column)
              style(dest_line)(dest_column) = source_style
          }

          found = null

          val item00 = target(lidx)(ridx)
          val item10 = target(lidx+1)(ridx)

          if (smartSet(0)(0).contains(item00) &&
              smartSet(1)(0).contains(item10)) {
            val tempArray = Array.ofDim[Char](2)
            tempArray(0) = item00
            tempArray(1) = item10

            found = substitutions.getOrElse(tempArray.view, null)
          }
        }

        ridx +=1
      }
      lidx +=1
    }
  }

  def replace_list_2x2(target: Array[Array[Char]],
                      substitutions: Map[SqueezedMatrixView,(Array[((Int, Int), Char)], ((Int, Int), (Int, Int)))],
                      max_column: Int = -1) {
    val expected_size = (2, 2)

    val smartSet = getSmartSet(substitutions.keySet, expected_size)

    var lidx = 0
    var ridx = 0
    while (lidx < (target.size - 1)) {
      val max_possible_col_idx = target(lidx).size -1
      val inner_max_column = if (max_column != -1 && max_column < max_possible_col_idx ) {
        max_column + 1
      } else {
        max_possible_col_idx
      }

      ridx = 0
      while (ridx < inner_max_column) {
        var found: (Array[((Int, Int), Char)], ((Int,Int), (Int, Int))) = null

        val nextLineIsValid = ridx + 1 < target(lidx+1).size

        if (nextLineIsValid) {
          val item00 = target(lidx)(ridx)
          val item01 = target(lidx)(ridx+1)
          val item10 = target(lidx+1)(ridx)
          val item11 = target(lidx+1)(ridx + 1)

          if (smartSet(0)(0).contains(item00) &&
              smartSet(0)(1).contains(item01) &&
              smartSet(1)(0).contains(item10) &&
              smartSet(1)(1).contains(item11)) {

            val tempArray = Array.ofDim[Char](4)
            tempArray(0) = item00
            tempArray(1) = item01
            tempArray(2) = item10
            tempArray(3) = item11

            found = substitutions.getOrElse(tempArray.view, null)
          }
        }

        while (found != null) {
          val pair = found

          val replacement = pair._1

          var paint = pair._2
          var k = 0
          while (k < replacement.size) {
            val ((x, y), c) = replacement(k)
            target(lidx + x)(ridx + y) = c
            k += 1
          }

          if (paint != null) {
              val (source, dest) = paint
              val source_line = lidx + source._1
              val source_column = ridx + source._2
              val dest_line = lidx + dest._1
              val dest_column = ridx + dest._2
              val source_style = style(source_line)(source_column)
              style(dest_line)(dest_column) = source_style
          }

          found = null

          val item00 = target(lidx)(ridx)
          val item01 = target(lidx)(ridx+1)
          val item10 = target(lidx+1)(ridx)
          val item11 = target(lidx+1)(ridx + 1)

          if (smartSet(0)(0).contains(item00) &&
              smartSet(0)(1).contains(item01) &&
              smartSet(1)(0).contains(item10) &&
              smartSet(1)(1).contains(item11)) {
            val tempArray = Array.ofDim[Char](4)
            tempArray(0) = item00
            tempArray(1) = item01
            tempArray(2) = item10
            tempArray(3) = item11

            found = substitutions.getOrElse(tempArray.view, null)
          }
        }

        ridx +=1
      }
      lidx +=1
    }
  }

   def replace_list_2x3(target: Array[Array[Char]],
                      substitutions: Map[SqueezedMatrixView,(Array[((Int, Int), Char)], ((Int, Int), (Int, Int)))],
                      max_column: Int = -1) {
    val expected_size = (2, 3)

    val smartSet = getSmartSet(substitutions.keySet, expected_size)

    var lidx = 0
    var ridx = 0
    while (lidx < (target.size - 1)) {
      val max_possible_col_idx = target(lidx).size - 2
      val inner_max_column = if (max_column != -1 && max_column < max_possible_col_idx ) {
        max_column + 1
      } else {
        max_possible_col_idx
      }

      ridx = 0
      while (ridx < inner_max_column) {
        var found: (Array[((Int, Int), Char)], ((Int,Int), (Int, Int))) = null

        val nextLineIsValid = ridx + 2 < target(lidx+1).size

        if (nextLineIsValid) {
          val item00 = target(lidx)(ridx)
          val item01 = target(lidx)(ridx+1)
          val item02 = target(lidx)(ridx+2)
          val item10 = target(lidx+1)(ridx)
          val item11 = target(lidx+1)(ridx + 1)
          val item12 = target(lidx+1)(ridx + 2)

          if (smartSet(0)(0).contains(item00) &&
              smartSet(0)(1).contains(item01) &&
              smartSet(0)(2).contains(item02) &&
              smartSet(1)(0).contains(item10) &&
              smartSet(1)(1).contains(item11) &&
              smartSet(1)(2).contains(item12)) {

            val tempArray = Array.ofDim[Char](6)
            tempArray(0) = item00
            tempArray(1) = item01
            tempArray(2) = item02
            tempArray(3) = item10
            tempArray(4) = item11
            tempArray(5) = item12

            found = substitutions.getOrElse(tempArray.view, null)
          }
        }

        while (found != null) {
          val pair = found

          val replacement = pair._1

          var paint = pair._2
          var k = 0
          while (k < replacement.size) {
            val ((x, y), c) = replacement(k)
            target(lidx + x)(ridx + y) = c
            k += 1
          }

          if (paint != null) {
              val (source, dest) = paint
              val source_line = lidx + source._1
              val source_column = ridx + source._2
              val dest_line = lidx + dest._1
              val dest_column = ridx + dest._2
              val source_style = style(source_line)(source_column)
              style(dest_line)(dest_column) = source_style
          }

          found = null

          val item00 = target(lidx)(ridx)
          val item01 = target(lidx)(ridx+1)
          val item02 = target(lidx)(ridx+2)
          val item10 = target(lidx+1)(ridx)
          val item11 = target(lidx+1)(ridx + 1)
          val item12 = target(lidx+1)(ridx + 2)

          if (smartSet(0)(0).contains(item00) &&
              smartSet(0)(1).contains(item01) &&
              smartSet(0)(2).contains(item02) &&
              smartSet(1)(0).contains(item10) &&
              smartSet(1)(1).contains(item11) &&
              smartSet(1)(2).contains(item12)) {
            val tempArray = Array.ofDim[Char](6)
            tempArray(0) = item00
            tempArray(1) = item01
            tempArray(2) = item02
            tempArray(3) = item10
            tempArray(4) = item11
            tempArray(5) = item12

            found = substitutions.getOrElse(tempArray.view, null)
          }
        }

        ridx +=1
      }
      lidx +=1
    }
  }

def replace_list(origin_target: Array[Array[Char]],
                 substitutions: Map[SqueezedMatrixView,(Array[((Int, Int), Char)], ((Int, Int), (Int, Int)))],
                 expected_size: (Int, Int),
                 max_column: Int = -1): Array[Array[Char]] = {
    //println("replace_list")

    var target = origin_target

    if (expected_size == (2,1)) {
      replace_list_2x1(origin_target, substitutions, max_column)
      return target
    }
    if (expected_size == (2,2)) {
      replace_list_2x2(origin_target, substitutions, max_column)
      return target
    }
    if (expected_size == (2,3)) {
      replace_list_2x3(origin_target, substitutions, max_column)
      return target
    }

    // println("substitutions list")
    // substitutions.foreach { case (a, (b, _)) =>
    //     println("expected")
    //     println(a.map(_.mkString("")).mkString("\n"))
    //     println("replacement")
    //     println(b.map(_.mkString("")).mkString("\n"))
    // }
    //substitutions.map(x => (x._1.map(_.mkString("")).mkString("\n"), x._2._1.map(_.mkString("")).mkString("\n"))).foreach(println)

    //val expected_size = get_matrix_size(substitutions.keys.head)
    //val first_char_set = substitutions.keySet.map(m => m(0)(0))
    //println("SMART WILL BUILD")
    val smartSet = getSmartSet(substitutions.keySet, expected_size)

    //println("SMART SET BUILT")

    def smartContains(start_pos: (Int, Int)): Boolean = {
      val (start_x, start_y) = start_pos

      if (target.size < start_x + expected_size._1) {
        return false
      }

      var target_x = 0
      var target_y = 0

      var i = 0
      var j = 0

      while (i < expected_size._1) {
        target_x = start_x + i

        if (target(target_x).size < start_y + expected_size._2) {
          return false
        }

        j = 0
        while (j < expected_size._2) {
          target_y = start_y + j
          if (!smartSet(i)(j).contains(target(target_x)(target_y))) {
            return false
          }

          j += 1
        }

        i += 1
      }

     return true
    }

    //println(expected_size)
    var lidx = 0
    var ridx = 0
    //for (lidx <- 0 until target.size) {
    while (lidx < target.size) {
        //inner_max_column = target(lidx).size - (expected_size._2 - 1)
        val inner_max_column = if (max_column == -1) target(lidx).size  else max_column + 1
        //for (ridx <- 0 until inner_max_column) {
        ridx = 0
        while (ridx < inner_max_column) {
            //////println("replace_list 4 " + (lidx,ridx))
            // println("breakable--->")
            //breakable {
            // if (max_column != -1 && ridx > max_column) {
            //     // println("break!")
            //     break
            // }

            val start_pos = (lidx, ridx)
            //////println("start_pos=" + (lidx,ridx))
            var window: SqueezedMatrixView = null
            var found: (Array[((Int, Int), Char)], ((Int,Int), (Int, Int))) = null
            // if (true) {
            //if (first_char_set.contains(target(lidx)(ridx))) {
            //if (first_char_set.contains(target(lidx)(ridx)) && smartContains(start_pos)) {
            if (smartContains(start_pos)) {
                window = get_sub_matrix_view(target, start_pos, expected_size)
                found = substitutions.getOrElse(window, null)
            }
            //--// var foundTuple = substitutions.find(x => equals_matrix(target, start_pos)(x._1))
            //--// var found = foundTuple.map(_._2).getOrElse(null)
            //////println("found tuple")
            //////println(foundTuple.map(t => get_matrix_str(t._1)).getOrElse(""))
            //var found = substitutions.find(equals_matrix(target, start_pos))
            ////println("after_matrix")


            //#if expected_size[1] > 3:
            //#    print_matrix(window)
            //#    print()
            while (window != null && found != null) {
                        val pair = found
                        // println("replace_list 5" + pair)
                        val replacement = pair._1
                        // #print_matrix(replacement)
                        // println("replace matrix")
                        // println(replacement)
                        //target =
                        // println("replace_list 6 " + replacement.toSeq.map(_.toSeq))
                        //replace_matrix(replacement, target, start_pos)
                        // println("replace_list 7 " + window.toList.map(_.toList))

                        //var paintR = substitutions(window)
                        //println("replace_list 8" + paintR)
                        var paint = pair._2
                        var k = 0
                        while (k < replacement.size) {
                          val ((x, y), c) = replacement(k)
                          target(lidx + x)(ridx + y) = c
                          //target(lidx + replacement(k)._1._1)(ridx + replacement(k)._1._2) = replacement(k)._2
                          k += 1
                        }

                        // if (paint == null) {
                        //   replace_matrix(replacement, target, start_pos)
                        // }
                        // println("replace_list 9")
                        if (paint != null) {
                            val (source, dest) = paint
                            val source_line = lidx + source._1
                            val source_column = ridx + source._2
                            val dest_line = lidx + dest._1
                            val dest_column = ridx + dest._2
                            //style(dest_line)(dest_column) = style(source_line)(source_column).copy()
                            val source_style = style(source_line)(source_column)
                            //style(dest_line) = style(dest_line).updated(dest_column, source_style)
                            style(dest_line)(dest_column) = source_style
                            //style = style.updated(dest_line, )
                        }
                    //}

                    window = null
                    found = null
                    // if (true) {
                    // if (first_char_set.contains(target(lidx)(ridx))) {
                    // if (first_char_set.contains(target(lidx)(ridx)) && smartContains(start_pos)) {
                    if (smartContains(start_pos)) {
                        window = get_sub_matrix_view(target, start_pos, expected_size)
                        found = substitutions.getOrElse(window, null)
                    }
                   //found = null
                   //found = substitutions.find(x => equals_matrix(target, start_pos)(x._1)).map(_._2).getOrElse(null)
                   //foundTuple = substitutions.find(x => equals_matrix(target, start_pos)(x._1))
                   //found = foundTuple.map(_._2).getOrElse(null)
                //    println("found tuple")
                //    println(foundTuple.map(t => get_matrix_str(t._1)).getOrElse(""))
                //}
            }
        //}
            ridx +=1
        }
        lidx +=1
    }

    target
}

class GitLines(var lines: Array[Array[Char]]) {
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
        replace_list(this.lines, this.substitutions, this.expected_size, this.max_column)
        // replace_list_equals(this.lines, this.substitutions, this.max_column) //slower
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


val lines = new GitLines(graph_lines.toArray)


lines.paint()
lines.replace("/",
              "/").by("╭",
                      "╯")

lines.paint()
lines.replace("\\",
              "\\").by("╮",
                       "╰")


lines.run()
println("// Substitutions: " + took())
startMeasurament()

///==========================

mark()

lines.paint("S ",
            "D ")
lines.replace("| ",
              " ╮").by("| ",
                       "╰╮")

lines.paint()
lines.replace("|╯",
              "* ").by("├╯",
                       "* ")
lines.run()
print("Micro: 2x2 |=" + took())

//>=>=========================


lines.paint()
lines.replace("╰ ",
              "╭ ").by("| ",
                       "| ")

lines.paint("SD",
            "  ")
lines.replace("╰ ",
              " |").by("╰╮",
                       " |")

lines.paint("SD",
            "  ")
lines.replace("╰ ",
              " *").by("╰╮",
                       " *")

lines.paint("SD",
            "  ")
lines.replace("╰ ",
              " ╮").by("╰╮",
                       " |")

lines.run()
print(" ╰=" + took())

//>=>=========================

lines.paint("  ",
            "SD")
lines.replace(" *",
              "╭ ").by(" *",
                       "╭╯")

lines.paint("DS",
            "  ")
lines.replace(" ╯",
              "* ").by("╭╯",
                       "* ")

lines.paint("  ",
            "SD")
lines.replace(" |",
              "╭ ").by(" |",
                       "╭╯")

lines.paint("DS")
lines.replace(" ╯",
              "| ").by("╭╯",
                       "| ")

lines.paint("  ",
            "SD")
lines.replace(" ╯",
              "╭ ").by(" |",
                       "╭╯")

lines.run()
print(" ' '=" + took())

//>=>=========================

lines.paint(" D",
            " S")
lines.replace("* ",
              "|╮").by("*╮",
                       "||")
//lines.run()

///#????
///#lines.paint("  ",
///#            "SD")
///#lines.replace("|╯",
///#              "╭|").by("||",
///#                       "╭╯")




lines.run()
println(" *=" + took())
startMeasurament()

println("2x2 substitutions: " + tookFromMark())



//============================


lines.paint("D S",
            "   ")
lines.replace(" |╯",
              "╭| ").by("╭|╯",
                        "|| ")

lines.paint("D S",
            "   ")
lines.replace(" |─",
              "╭| ").by("╭|─",
                        "|| ")

//### new strategy

lines.paint()
lines.replace(" |╯",
              " | ").by(" ├╯",
                        " | ")

lines.paint()
lines.replace(" |╯",
              "╮| ").by(" ├╯",
                        "╮| ")



lines.run()
println("3x3 substitutions " + took())
startMeasurament()


// =================================

lines.set_maxcolumn(0)
lines.replace("|╭",
              "|╯").by("|╭",
                       "├╯")

lines.replace("||",
              "|╯").by("||",
                       "├╯")

lines.run()

println("last 2x2 substitutions " + took())


println("Global took: " + globalTook())

/*
 *  Too many escape codes since to make the line break before end of
 *  line is reached.
 *  With this function we try to compress style be reusing style of
 *  consecutive chars
 */
def compress_style(line_number: Int, line: Vector[Char]) = {
    //previous_idx = None
    //for idx, column in enumerate(line):
    //    if previous_idx is not None:
    //        if (style[line_number][previous_idx][0] == style[line_number][idx][0] and
    //                style[line_number][previous_idx][1] == style[line_number][idx][1]):
    //            style[line_number][previous_idx][1] = ""
    //            style[line_number][idx][0] = ""

    //    previous_idx = idx
}


def compress_escapes(line: String) = {
    //#return re.sub(r'(\x1b[[^m]*m)\x1bm(\x1b[[^m]*m)',
    //#              r'\1\2',
    //#              line)
    //final_line = line
    //#final_line = re.sub('\x1b\\[m(\x1b\\[[^m]+m)',
    //#                    r'\1',
    //#                    final_line)
    //#final_line = re.sub('\x1b\\[m(\x1b\\[[^m]+m)',
    //#                    r'\1',
    //#                    final_line)
    //final_line = re.sub('\x1b\\[m *\x1b\\[m',
    //                    '\x1b\\[m',
    //                    final_line)
    //final_line = re.sub('\x1b\\[m \\*\x1b\\[m',
    //                    '\x1b\\[m \\*',
    //                    final_line)

    //return final_line
    line
}


val final_ = lines.lines
for ((columns, line_number) <- final_.view.zipWithIndex) {
    //compress_style(line_number, columns)
    var line = ""
    var unstyled_line = ""
    for ((column, idx) <- columns.view.zipWithIndex) {
        breakable {
            if (idx > 80) {
                break
            }
            //#print(idx + column)
            //#print(idx + column)
            //#line += style[line_number][idx] + column
            unstyled_line += column
            //#if column == " ":
            //#    #line += column
            //#    line += style[line_number][idx][0] + column
            //#else:
            //#    #line += style[line_number][idx][0] + column + style
            line = line + style(line_number)(idx)._1 + column + style(line_number)(idx)._2
        }
    }

    line = line //+ messages(line_number)

    var not_empty_line = (unstyled_line.replace("|", "").replace("*", "").replace("", "")).size > 0
    not_empty_line = true
    if (not_empty_line) {
        line = line.replace('|', '│')
        //#line = line.replace('*', '┿')
        //#line = line.replace('|', 'H')
        line = compress_escapes(line)
        //#line = line.replace('\u001b', '\u001b')
        //#print(line + "<<" + str(len(line)), end='')
        //#print(line, end='')
        //#print()
        //#print(line + " <<" + str(len(line)) +"-"+ str(len(unstyled_line)))
        println(line)
        //#print(line[:40])
    }
}


//# good_
//# good chars for dot:
//# ┿
//# ╪
//# ┯
//# ╿
//# ┃

}}