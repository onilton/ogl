package utils

import scala.collection.mutable

object AnsiEscapeCodes {
  def ansiEscape(value: String): String = s"\u001b[" + value +"m"
  def ansiEscape(value: Int): String = ansiEscape(value.toString())
  def color(color: String) = ansiEscape(s"38;5;${color}")
  def color(color: Int) = ansiEscape(s"38;5;${color}")
  def invert = ansiEscape(7)
  def underline = ansiEscape(4)
  def reset = ansiEscape(0)

  def parseLine(line: String): (Array[(String, String)], String) = {
    var escapes: mutable.ArrayBuffer[(String, String)] = mutable.ArrayBuffer()
    var escapes_so_far = ""
    var clean_line: StringBuilder =  new StringBuilder()

    var index = 0
    var clean_index = 0
    var previousWasEscape = false
    while (index < line.size) {
      if (line(index) == '\u001b') {
        val start = line.indexOf("\u001b[", index)
        if (start != 1) {
          val end = line.indexOf("m", start)
          if ((end - start) <= 2) {
            if (previousWasEscape) {
              escapes_so_far += line.slice(start,end + 1)
            } else {
              escapes(escapes.size-1) = (escapes(escapes.size -1)._1, line.substring(start, end+1))
            }
          } else {
            escapes_so_far += line.slice(start,end + 1)
          }
          index = end
        }
        previousWasEscape = true
      } else {
        //escapes = escapes :+ (escapes_so_far, "")
        escapes.append((escapes_so_far, ""))
        escapes_so_far = ""
        clean_line.append(line(index))
        clean_index += 1
        previousWasEscape = false
      }
      index = index + 1
    }

    (escapes.toArray, clean_line.toString())
  }
}