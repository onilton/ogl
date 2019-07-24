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

  def getSubMatrixView(target: Array[Array[Char]],
                       startPos: (Int, Int),
                       size: (Int, Int)): SqueezedMatrixView = {
    val (startX, startY) = startPos
    val (height, width) = size
    val endY = startY + width

    val targetHeight = target.size

    var tempArray = Array.ofDim[Char](height*width)
    var view = tempArray.view

    var i = 0
    var j = 0
    var column = 0
    while (i < height) {
        j = 0
        while (j < width) {
            tempArray(column +  j) = target(startX + i)(startY + j)
            j+=1
        }
        column += j
        i+=1
    }

    return view
  }

  def replaceMatrix(replacement: CharMatrixView,
                    target: Array[Array[Char]],
                    startPos: (Int, Int)) = {
    val (startX, startY) = startPos

    var i = 0
    var j = 0
    while (i < replacement.size) {
      j = 0
      while (j < replacement(i).size) {
        target(startX + i)(startY + j) = replacement(i)(j)
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

  private def replaceList2x1(target: Array[Array[Char]],
                      substitutions: Map[SqueezedMatrixView,(Array[((Int, Int), Char)], ((Int, Int), (Int, Int)))],
                      style: Style,
                      maxColumn: Int = -1) {
    val expectedSize = (2, 1)

    val smartSet = getSmartSet(substitutions.keySet, expectedSize)

    var lidx = 0
    var ridx = 0
    while (lidx < (target.size - 1)) {
      val maxPossibleColIdx = target(lidx).size
      val innerMaxColumn = if (maxColumn != -1 && maxColumn < maxPossibleColIdx ) {
        maxColumn + 1
      } else {
        maxPossibleColIdx
      }

      ridx = 0
      while (ridx < innerMaxColumn) {
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
              val sourceLine = lidx + source._1
              val sourceColumn = ridx + source._2
              val destLine = lidx + dest._1
              val destColumn = ridx + dest._2
              val sourceStyle = style(sourceLine)(sourceColumn)
              style(destLine)(destColumn) = sourceStyle
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

  private def replaceList2x2(target: Array[Array[Char]],
                       substitutions: Map[SqueezedMatrixView,(Array[((Int, Int), Char)], ((Int, Int), (Int, Int)))],
                       style: Style,
                       maxColumn: Int = -1) {
    val expectedSize = (2, 2)

    val smartSet = getSmartSet(substitutions.keySet, expectedSize)

    var lidx = 0
    var ridx = 0
    while (lidx < (target.size - 1)) {
      val maxPossibleColIdx = target(lidx).size -1
      val innerMaxColumn = if (maxColumn != -1 && maxColumn < maxPossibleColIdx ) {
        maxColumn + 1
      } else {
        maxPossibleColIdx
      }

      ridx = 0
      while (ridx < innerMaxColumn) {
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
              val sourceLine = lidx + source._1
              val sourceColumn = ridx + source._2
              val destLine = lidx + dest._1
              val destColumn = ridx + dest._2
              val sourceStyle = style(sourceLine)(sourceColumn)
              style(destLine)(destColumn) = sourceStyle
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

  private def replaceList2x3(target: Array[Array[Char]],
                       substitutions: Map[SqueezedMatrixView,(Array[((Int, Int), Char)], ((Int, Int), (Int, Int)))],
                       style: Style,
                       maxColumn: Int = -1) {
    val expectedSize = (2, 3)

    val smartSet = getSmartSet(substitutions.keySet, expectedSize)

    var lidx = 0
    var ridx = 0
    while (lidx < (target.size - 1)) {
      val maxPossibleColIdx = target(lidx).size - 2
      val innerMaxColumn = if (maxColumn != -1 && maxColumn < maxPossibleColIdx ) {
        maxColumn + 1
      } else {
        maxPossibleColIdx
      }

      ridx = 0
      while (ridx < innerMaxColumn) {
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
              val sourceLine = lidx + source._1
              val sourceColumn = ridx + source._2
              val destLine = lidx + dest._1
              val destColumn = ridx + dest._2
              val sourceStyle = style(sourceLine)(sourceColumn)
              style(destLine)(destColumn) = sourceStyle
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

  private def replaceListNxN(originTarget: Array[Array[Char]],
                 substitutions: Map[SqueezedMatrixView,(Array[((Int, Int), Char)], ((Int, Int), (Int, Int)))],
                 expectedSize: (Int, Int),
                 style: Style,
                 maxColumn: Int = -1): Array[Array[Char]] = {
    //println("replace_list")

    var target = originTarget

    // println("substitutions list")
    // substitutions.foreach { case (a, (b, _)) =>
    //     println("expected")
    //     println(a.map(_.mkString("")).mkString("\n"))
    //     println("replacement")
    //     println(b.map(_.mkString("")).mkString("\n"))
    // }
    //substitutions.map(x => (x._1.map(_.mkString("")).mkString("\n"), x._2._1.map(_.mkString("")).mkString("\n"))).foreach(println)

    //val expectedSize = get_matrix_size(substitutions.keys.head)
    //val first_char_set = substitutions.keySet.map(m => m(0)(0))
    //println("SMART WILL BUILD")
    val smartSet = getSmartSet(substitutions.keySet, expectedSize)

    //println("SMART SET BUILT")

    def smartContains(startPos: (Int, Int)): Boolean = {
      val (startX, startY) = startPos

      if (target.size < startX + expectedSize._1) {
        return false
      }

      var targetX = 0
      var targetY = 0

      var i = 0
      var j = 0

      while (i < expectedSize._1) {
        targetX = startX + i

        if (target(targetX).size < startY + expectedSize._2) {
          return false
        }

        j = 0
        while (j < expectedSize._2) {
          targetY = startY + j
          if (!smartSet(i)(j).contains(target(targetX)(targetY))) {
            return false
          }

          j += 1
        }

        i += 1
      }

     return true
    }

    //println(expectedSize)
    var lidx = 0
    var ridx = 0
    //for (lidx <- 0 until target.size) {
    while (lidx < target.size) {
        //inner_maxColumn = target(lidx).size - (expectedSize._2 - 1)
        val innerMaxColumn = if (maxColumn == -1) target(lidx).size  else maxColumn + 1
        //for (ridx <- 0 until innerMaxColumn) {
        ridx = 0
        while (ridx < innerMaxColumn) {
            //////println("replace_list 4 " + (lidx,ridx))
            // println("breakable--->")
            //breakable {
            // if (maxColumn != -1 && ridx > maxColumn) {
            //     // println("break!")
            //     break
            // }

            val startPos = (lidx, ridx)
            //////println("startPos=" + (lidx,ridx))
            var window: SqueezedMatrixView = null
            var found: (Array[((Int, Int), Char)], ((Int,Int), (Int, Int))) = null
            // if (true) {
            //if (first_char_set.contains(target(lidx)(ridx))) {
            //if (first_char_set.contains(target(lidx)(ridx)) && smartContains(startPos)) {
            if (smartContains(startPos)) {
                window = getSubMatrixView(target, startPos, expectedSize)
                found = substitutions.getOrElse(window, null)
            }
            //--// var foundTuple = substitutions.find(x => equals_matrix(target, startPos)(x._1))
            //--// var found = foundTuple.map(_._2).getOrElse(null)
            //////println("found tuple")
            //////println(foundTuple.map(t => get_matrix_str(t._1)).getOrElse(""))
            //var found = substitutions.find(equals_matrix(target, startPos))
            ////println("after_matrix")


            //#if expectedSize[1] > 3:
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
                        //replaceMatrix(replacement, target, start_pos)
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
                        //   replaceMatrix(replacement, target, start_pos)
                        // }
                        // println("replace_list 9")
                        if (paint != null) {
                            val (source, dest) = paint
                            val sourceLine = lidx + source._1
                            val sourceColumn = ridx + source._2
                            val destLine = lidx + dest._1
                            val destColumn = ridx + dest._2
                            //style(destLine)(destColumn) = style(sourceLine)(sourceColumn).copy()
                            val sourceStyle = style(sourceLine)(sourceColumn)
                            //style(destLine) = style(destLine).updated(destColumn, sourceStyle)
                            style(destLine)(destColumn) = sourceStyle
                            //style = style.updated(destLine, )
                        }
                    //}

                    window = null
                    found = null
                    // if (true) {
                    // if (first_char_set.contains(target(lidx)(ridx))) {
                    // if (first_char_set.contains(target(lidx)(ridx)) && smartContains(startPos)) {
                    if (smartContains(startPos)) {
                        window = getSubMatrixView(target, startPos, expectedSize)
                        found = substitutions.getOrElse(window, null)
                    }
                   //found = null
                   //found = substitutions.find(x => equals_matrix(target, startPos)(x._1)).map(_._2).getOrElse(null)
                   //foundTuple = substitutions.find(x => equals_matrix(target, startPos)(x._1))
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


  def replaceList(target: Array[Array[Char]],
                   substitutions: Map[SqueezedMatrixView,(Array[((Int, Int), Char)], ((Int, Int), (Int, Int)))],
                   expectedSize: (Int, Int),
                   style: Style,
                   maxColumn: Int = -1): Array[Array[Char]] = {
    if (expectedSize == (2,1)) {
      replaceList2x1(target, substitutions, style, maxColumn)
      return target
    }
    if (expectedSize == (2,2)) {
      replaceList2x2(target, substitutions, style, maxColumn)
      return target
    }
    if (expectedSize == (2,3)) {
      replaceList2x3(target, substitutions, style, maxColumn)
      return target
    }
    return replaceListNxN(target, substitutions, expectedSize, style, maxColumn)
  }

}