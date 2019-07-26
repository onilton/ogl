import scala.io.Source
import util.control.Breaks._
import scala.collection.mutable
import EasyMetrics._
import java.lang.ProcessBuilder
import scala.util.Try
import java.nio.charset.CodingErrorAction
import scala.io.Codec
import CharMatrixOps.replaceList




object ogl {

  //def runCommand(Se)

  def main(args: Array[String]): Unit = {
    val debugEnabled = args.contains("--debug")
    val boldEnabled = args.contains("--bold")
    val d = Debugger(debugEnabled)

    d.debug("Started")
    startMeasurament()
    //val data1 = Source.fromFile("um_tempcolor", "utf-8").getLines
    // Remember a line is broken(encoding)
    // better iterate in loop to avoid errors
    //val data1 = Source.fromFile("um_tempcolor", "utf-8").getLines.toArray

    val gitCommand = Seq(
      "git",
      "log",
      "--graph",
      //"--pretty=\"format:\\%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset\"",
      "--pretty=format:%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset",
      //"--pretty=format:%h -%d %s (%cr) <%an>",
      "--abbrev-commit",
      "--color"
    ) ++ args.filterNot(_ == "--debug").filterNot(_ == "--bold").toSeq

    val proc = new ProcessBuilder(gitCommand: _*).start()
    implicit val codec = Codec("UTF-8")
    codec.onMalformedInput(CodingErrorAction.REPLACE)
    codec.onUnmappableCharacter(CodingErrorAction.REPLACE)
    val out  = Source.fromInputStream(proc.getInputStream).getLines.toArray
    proc.waitFor
    val data1 = out
    //val data1 = out.mkString.split("\n")
    d.debug("ok")


    d.debug("File load " + took())

    val allGraphChars = Set('*','|','\\','/',' ','_')

    var style: mutable.ArrayBuffer[Array[(String, String)]] =
        new mutable.ArrayBuffer[Array[(String, String)]](data1.size)
    var graph_lines: mutable.ArrayBuffer[Array[Char]] = new mutable.ArrayBuffer[Array[Char]](data1.size)
    var messages: mutable.ArrayBuffer[String] = new mutable.ArrayBuffer[String](data1.size)

    for (raw_line <- data1) {
      val (escapes, line) = parseAnsiEscapeCodes(raw_line)

      val index = line.indexWhere(c => !allGraphChars.contains(c))

      val (graph, message) =
        if (index >= 0) {
          val last_graph_idx = index
          line.splitAt(last_graph_idx)
        } else {
          (line, "")
        }

      if (graph.exists(c => c == '/' || c == '\\')) {
        val commitCharIdx = graph.indexOf("*")
        var extended = graph
        if (commitCharIdx >= 0) {
          graph_lines.lastOption.foreach { lastLine =>
            if (commitCharIdx < lastLine.size && commitCharIdx > 1) {
              if (lastLine(commitCharIdx) == '|') {
                escapes(commitCharIdx) = style.last(commitCharIdx)
              } else if (lastLine(commitCharIdx -1) == '\\') {
                escapes(commitCharIdx) = style.last(commitCharIdx-1)
              }
            }
          }
          extended = extended.updated(commitCharIdx, '|')
        }

        extended = extended.replace("_", " ")
        style.append(escapes)
        graph_lines.append(extended.toArray)
        messages.append("")
      }

      graph_lines.append(graph.replace("|_", "|─").toArray)
      style.append(escapes.toArray)
      messages.append(message)
    }

    d.debugNoNL("Summary | lines: " + graph_lines.size)
    d.debug(" | original lines: " + data1.size)
    d.debug("Splitted graph " + took())
    startMeasurament()


    val lines = new GitGraphReplacer(graph_lines.toArray, style)


    lines.paint()
    lines.replace("/",
                  "/").by("╭",
                          "╯")

    lines.paint()
    lines.replace("\\",
                  "\\").by("╮",
                          "╰")


    lines.run()
    d.debug("// Substitutions: " + took())
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
    d.debugNoNL("Micro: 2x2 |=" + took())

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
    d.debugNoNL(" ╰=" + took())

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
    d.debugNoNL(" ' '=" + took())

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
    d.debug(" *=" + took())
    startMeasurament()

    d.debug("2x2 substitutions: " + tookFromMark())



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
    d.debug("3x3 substitutions " + took())
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

    d.debug("last 2x2 substitutions " + took())

    // =========================

    val r = scala.util.Random

    var lidx = 0
    for (line <- graph_lines) {
      var ridx = 0
      for (char <- line) {
        if (char == '*') {
          if (lidx - 1 < 0 ||
              ridx >= graph_lines(lidx-1).size ||
              graph_lines(lidx-1)(ridx) == ' ' ) {
            graph_lines(lidx)(ridx) = '┬'
            r.setSeed(lidx + ridx + 3)
            val randomEscapeColor =  "\u001b[38;5;" + (r.nextInt(228) + 1) + "m"
            style(lidx)(ridx) = style(lidx)(ridx).copy(
              _1 = randomEscapeColor
            )
          }

        }
        ridx +=1
      }
      lidx +=1
    }

    d.debug("Childless commits " + took())

    // =========================

    lines.paint("S",
                "D")
    lines.replace("┬",
                  "*").by("┬",
                          "*")

    lines.paint("S",
                "D")
    lines.replace("╮",
                  "*").by("╮",
                          "*")

    lines.paint("S",
                "D")
    lines.replace("|",
                  "*").by("|",
                          "*")

    lines.paint("S",
                "D")
    lines.replace("*",
                  "*").by("*",
                          "*")

    lines.paint("S",
                "D")
    lines.replace("├",
                  "*").by("├",
                          "*")

    lines.paint("S",
                "D")
    lines.replace("╭",
                  "*").by("╭",
                          "*")

    // lines.paint()
    // lines.replace("*",
    //               "|").by("╬",
    //                       "|")

    lines.run()

    d.debug("Paint * 2x1 " + took())

    d.debug("Global took: " + globalTook())

    //System.exit(0)

    val final_ = lines.lines
    for ((columns, line_number) <- final_.view.zipWithIndex) {
        //compress_style(line_number, columns)
        var line = ""
        var unstyled_line = ""
        var commitColor = ""
        var not_empty_line = false
        for ((column, idx) <- columns.view.zipWithIndex) {
          breakable {
            if (idx > 80) {
                break
            }
            if (column != '|' && column != ' ') {
              not_empty_line = true
            }
            //#print(idx + column)
            //#print(idx + column)
            //#line += style[line_number][idx] + column
            // unbold
            if (style(line_number)(idx)._1.startsWith("\u001b[1;")) {
              val codeStr = style(line_number)(idx)._1.replace("\u001b[1;", "").replace("m", "")
              val codeNumber = codeStr.toInt - 22
              style(line_number)(idx) = style(line_number)(idx).copy(
                _1 = "\u001b[38;5;" + codeNumber + "m")
            }
            if (column == '*') {
              commitColor =  style(line_number)(idx)._1
            }

            unstyled_line += column
            //#if column == " ":
            //#    #line += column
            //#    line += style[line_number][idx][0] + column
            //#else:
            //#    #line += style[line_number][idx][0] + column + style
            line = line + style(line_number)(idx)._1 + column + style(line_number)(idx)._2
          }
        }
        var message = ""
        var idx = columns.size
        var firstBlankFound = false
        for (c <- messages(line_number).toArray) {
          if (c == ' ') {
            firstBlankFound = true
          }
          if (firstBlankFound) {
            message = message + style(line_number)(idx)._1 + c + style(line_number)(idx)._2
          } else {
            message = message + commitColor + c + style(line_number)(idx)._2
          }
          idx += 1
        }

        line = line + message

        not_empty_line = true
        if (not_empty_line) {
          line = line.replace('|', '│')
          //line = line.replace('*', '┿')
          //line = line.replace('*', '┷')
          //line = line.replace('*', '┙')
          //line = line.replace('*', '┥')
          //line = line.replace('*', '═')
          line = line.replace('*', '╪')

          //line = line.replace('*', '╬')





          //#line = line.replace('|', 'H')
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

  }

  def parseAnsiEscapeCodes(line: String): (Array[(String, String)], String) = {
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