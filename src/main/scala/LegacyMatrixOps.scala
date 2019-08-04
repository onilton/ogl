object LegacyMatrixOps {
  type CharMatrix = Array[Array[Char]]
  type CharMatrixView = scala.collection.SeqView[scala.collection.mutable.IndexedSeqView[Char,Array[Char]],Array[scala.collection.mutable.IndexedSeqView[Char,Array[Char]]]]

  def get_matrix_size(matrix: CharMatrixView) = (matrix.size, matrix(0).size)

  def get_matrix_str(matrix: CharMatrixView) = matrix.map(_.mkString("")).mkString("\n")

  def get_sub_matrix(target: Array[Array[Char]], start_pos: (Int, Int) , size: (Int, Int)): Vector[Vector[Char]] = {
    val (start_x, start_y) = start_pos
    val (height, width) = size
    val end_y = start_y + width

    val target_height = target.size

    if (start_x + height > target_height)
        return null

    var window = Vector.empty[Vector[Char]]

    for (offset_x <- 0 until height) {
        val x = start_x + offset_x

        if (end_y > target(x).size) {
            return null
        }

        // optimize??
        window = window :+ target(x).slice(start_y,end_y).toVector
        //window = window :+ tuple(target(x)[start_y:end_y])
        // #window[offset_x] = target(x)[start_y:end_y]
    }

    return window
  }

  def equals_matrix(target: CharMatrix, start_pos: (Int, Int))(expected: CharMatrixView): Boolean = {
    val (start_x, start_y) = start_pos
    // if (start_pos == (9,5)) println("expected" + expected.toList.map(_.toList))

    // if (start_pos == (9,5)) println("comparsion" +
    //     target.slice(start_x,start_x + expected.size).map(_.drop(start_y)).toList.map(_.toList))

    // if (start_pos == (9,5)) println("comparsion2" +
    //     target.slice(start_x,start_x + expected.size).toList.map(_.toList) + "|")

    if (target.size < start_x + expected.size) {
        return false
    }

   var target_x = 0
   //for (i <- 0 until expected.size) {
    var i = 0
    var j = 0
    while (i < expected.size) {
    // if (start_pos == (9,5)) println("debug=" + i)
        target_x = start_x + i
        if (target(target_x).size < start_y + expected(i).size) {
            // if (start_pos == (9,5)) println("give up column too big")
            return false
        }
        // if (start_pos == (9,5)) println("debug target size =" + target(i).size)
        // if (start_pos == (9,5)) println("debug going size =" + start_y + "+"+ expected(i).size)
        //for (j <- 0 until expected(i).size) {
        j = 0
        while (j < expected(i).size) {
            // if (start_pos == (9,5)) println("debug" + (i,j))
           if (target(target_x)(start_y + j) != expected(i)(j) ) {
               return false
           }
        //    if (start_pos == (9,5)) println("after debug" + (i,j))
           j += 1
        }
        i += 1
    }
    // if (start_pos == (9,5)) println("out")

    return true
  }

  def replace_matrix(replacement: CharMatrixView,
                   target: Array[Array[Char]],
                   start_pos: (Int, Int)) = {
    val (start_x, start_y) = start_pos

    val new_target = target

    // println("replace")
    // println(replacement)
    // println(start_pos   )

    var i = 0
    var j = 0
    //for (i <- 0 until replacement.size) {
    while (i < replacement.size) {
        // for (j <- 0 until replacement(i).size) {
        j = 0
        while (j < replacement(i).size) {
            //target(start_x + i)(start_y + j) = replacement(i)(j)
            // new_target = new_target.updated(
            //     start_x + i,
            //     new_target(start_x + i).updated(start_y + j, replacement(i)(j)))
            //new_target(start_x + i) = new_target(start_x + i).updated(start_y + j, replacement(i)(j))
            new_target(start_x + i)(start_y + j) = replacement(i)(j)
            j+=1
        }

        i+=1
    }
    // println(new_target.slice(start_x, start_x + len(replacement)))
    new_target
    //updateTarget(new_target)
  }

  def replace_list_equals(origin_target: Array[Array[Char]],
                        substitutions: Map[CharMatrixView,(CharMatrixView, ((Int, Int), (Int, Int)))],
                        expected_size: (Int, Int),
                        max_column: Int = -1) = {
    var target = origin_target
    //val expected_size = get_matrix_size(substitutions.values.toIndexedSeq(0)._1)
    val first_char_set = substitutions.keySet.map(m => m(0)(0))
    val substitutionsArray = substitutions.toArray
    var lidx = 0
    var ridx = 0

    while (lidx < target.size) {
      val inner_max_column = if (max_column == -1) target(lidx).size  else max_column + 1
      ridx = 0
      while (ridx < inner_max_column) {
        val start_pos = (lidx, ridx)
        var found: (CharMatrixView, ((Int,Int), (Int, Int))) = null
        if (first_char_set.contains(target(lidx)(ridx))) {
          found = substitutionsArray.find(x => equals_matrix(target, start_pos)(x._1)).map(_._2).getOrElse(null)
          //found = findInArray(target, substitutionsArray, start_pos, expected_size)
        }

        while (found != null) {
          val pair = found
          val replacement = pair._1
          replace_matrix(replacement, target, start_pos)

          var paint = pair._2

          if (paint != null) {
              val (source, dest) = paint
              val source_line = lidx + source._1
              val source_column = ridx + source._2
              val dest_line = lidx + dest._1
              val dest_column = ridx + dest._2
              //val source_style = style(source_line)(source_column)
              //style(dest_line)(dest_column) = source_style
          }

          found = null
          if (first_char_set.contains(target(lidx)(ridx))) {
            found = substitutions.find(x => equals_matrix(target, start_pos)(x._1)).map(_._2).getOrElse(null)
            //found = findInArray(target, substitutionsArray, start_pos, expected_size)
          }
        }
        ridx +=1
      }
      lidx +=1
    }

    target
  }

  /* Seems to be slower */
  def findInArray(target: CharMatrix,
                  array: Array[(CharMatrixView,(CharMatrixView, ((Int, Int), (Int, Int))))],
                  start_pos: (Int, Int), size: (Int, Int)): (CharMatrixView, ((Int, Int), (Int, Int))) = {
    val (lidx, ridx) = start_pos
    val (height, width) = size
    var i = 0
    var found = null
    while (i < array.size) {
    if (equals_matrix(target, start_pos)(array(i)._1)) {
      return array(i)._2
    }

    i+=1
    }

    return found
  }
}