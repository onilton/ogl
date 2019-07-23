import scala.collection.mutable

/*
 * Here lies dirty, ugly, mutable, side-effectful but efficient code to deal
 * with Char matrixes
 */
object CharMatrixOps {
  type CharMatrix = Array[Array[Char]]

  type CharMatrixView = scala.collection.SeqView[scala.collection.mutable.IndexedSeqView[Char,Array[Char]],Array[scala.collection.mutable.IndexedSeqView[Char,Array[Char]]]]
  type SqueezedMatrixView = scala.collection.SeqView[Char,Array[Char]]

  type Style = mutable.ArrayBuffer[Array[(String, String)]]

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

  private def replace_list_2x1(target: Array[Array[Char]],
                      substitutions: Map[SqueezedMatrixView,(Array[((Int, Int), Char)], ((Int, Int), (Int, Int)))],
                      style: Style,
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

  private def replace_list_2x2(target: Array[Array[Char]],
                       substitutions: Map[SqueezedMatrixView,(Array[((Int, Int), Char)], ((Int, Int), (Int, Int)))],
                       style: Style,
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

  private def replace_list_2x3(target: Array[Array[Char]],
                       substitutions: Map[SqueezedMatrixView,(Array[((Int, Int), Char)], ((Int, Int), (Int, Int)))],
                       style: Style,
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

  private def replace_list_nxn(origin_target: Array[Array[Char]],
                 substitutions: Map[SqueezedMatrixView,(Array[((Int, Int), Char)], ((Int, Int), (Int, Int)))],
                 expected_size: (Int, Int),
                 style: Style,
                 max_column: Int = -1): Array[Array[Char]] = {
    //println("replace_list")

    var target = origin_target

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


  def replace_list(target: Array[Array[Char]],
                   substitutions: Map[SqueezedMatrixView,(Array[((Int, Int), Char)], ((Int, Int), (Int, Int)))],
                   expected_size: (Int, Int),
                   style: Style,
                   max_column: Int = -1): Array[Array[Char]] = {
    if (expected_size == (2,1)) {
      replace_list_2x1(target, substitutions, style, max_column)
      return target
    }
    if (expected_size == (2,2)) {
      replace_list_2x2(target, substitutions, style, max_column)
      return target
    }
    if (expected_size == (2,3)) {
      replace_list_2x3(target, substitutions, style, max_column)
      return target
    }
    return replace_list_nxn(target, substitutions, expected_size, style, max_column)
  }

}