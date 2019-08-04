package utils

import scala.collection.mutable

object AnsiEscapeCodes {
  def ansiEscape(value: String): String = s"\u001b[" + value +"m"
  def ansiEscape(value: Int): String = ansiEscape(value.toString())
  def color(color: String) = ansiEscape(s"38;5;${color}")
  def color(color: Int) = ansiEscape(s"38;5;${color}")
  def invert = ansiEscape(7)
  def underline = ansiEscape(4)
  def bold = ansiEscape(1)
  def reset = ansiEscape(0)

  def parseLine(line: String): (Array[(String, String)], String) = {
    var escapes: mutable.ArrayBuffer[(String, String)] = mutable.ArrayBuffer()
    var escapesSoFar = ""
    var cleanLine: StringBuilder =  new StringBuilder()

    var index = 0
    var previousWasEscape = false
    while (index < line.size) {
      if (line(index) == '\u001b') {
        val start = line.indexOf("\u001b[", index)
        if (start != 1) {
          val end = line.indexOf("m", start)
          if ((end - start) <= 2) {
            if (previousWasEscape) {
              escapesSoFar += line.slice(start,end + 1)
            } else {
              escapes(escapes.size-1) = (escapes(escapes.size -1)._1, line.substring(start, end+1))
            }
          } else {
            escapesSoFar += line.slice(start,end + 1)
          }
          index = end
        }
        previousWasEscape = true
      } else {
        //escapes = escapes :+ (escapesSoFar, "")
        escapes.append((escapesSoFar, ""))
        escapesSoFar = ""
        cleanLine.append(line(index))
        previousWasEscape = false
      }
      index = index + 1
    }

    (escapes.toArray, cleanLine.toString())
  }
}