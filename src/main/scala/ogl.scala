import scala.io.Source
import util.control.Breaks._
import scala.collection.mutable
import EasyMetrics._
import java.lang.ProcessBuilder
import scala.util.Try
import java.nio.charset.CodingErrorAction
import scala.io.Codec
import CharMatrixOps.replaceList
import java.lang.ProcessBuilder.Redirect
import java.io.OutputStreamWriter
import config.{ ArgParser, Config }
import config.ConfigFile




object ogl {

  def main(args: Array[String]): Unit = {
    val argParser = ArgParser(args)
    val config = Config.getConfig(ConfigFile.getPartialConfig.toList ++ List(argParser.partialConfig))

    val d = Debugger(config.debugEnabled)

    d.debug("Started")
    startMeasurament()
    //val data1 = Source.fromFile("um_tempcolor", "utf-8").getLines
    // Remember a line is broken(encoding)
    // better iterate in loop to avoid errors
    //val data1 = Source.fromFile("um_tempcolor", "utf-8").getLines.toArray

    val format =
      if (config.unlimitedFields)
        GitLogGraph.simpleFormat(config.subjectColor, config.authorNameColor, config.commitDateColor)
      else
        GitLogGraph.fixedWidthFormat(config.subjectColor, config.authorNameColor, config.commitDateColor)(config.subjectWidth, config.authorNameWidth, config.commitDateWidth)

    val gitLogGraph = GitLogGraph(format, argParser.gitArgs)
    if (gitLogGraph.failed) {
      if (gitLogGraph.errorOut.nonEmpty) {
        gitLogGraph.errorOut.foreach(println)
      } else {
        println("Git command failed!")
      }
      System.exit(1)
    }
    val data1 = gitLogGraph.out

    d.debug("File load " + took())


    val allGraphChars = Set('*','|','\\','/',' ','_')

    var style: mutable.ArrayBuffer[Array[(String, String)]] =
        new mutable.ArrayBuffer[Array[(String, String)]](data1.size)
    var graph_lines: mutable.ArrayBuffer[Array[Char]] = new mutable.ArrayBuffer[Array[Char]](data1.size)
    var messages: mutable.ArrayBuffer[String] = new mutable.ArrayBuffer[String](data1.size)
    var maxGraphLine = 0

    for (raw_line <- data1) {
      val (escapes, line) = parseAnsiEscapeCodes(raw_line)

      val index = line.indexWhere(c => !allGraphChars.contains(c))

      val (graph, message) =
        if (index >= 0) {
          val last_graph_idx = index
          if (last_graph_idx > maxGraphLine) {
            maxGraphLine = last_graph_idx
          }
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

    addColorToChildlessCommits(graph_lines, style, config.seed)

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


    if (config.verticalShrink >= 2) {
      lines.paint()
      lines.replace("||",
                    "├╯").by("├╯",
                             "| ")
      lines.run()

      lines.paint()
      lines.replace("|╭",
                    "├╯").by("├─",
                    "| ")
      lines.run()

      d.debug("Vertical shrink 2 " + took())
    }

    d.debug("Global took: " + globalTook())

    //System.exit(0)

    val pagerCommand = Seq(
      "less",
      "-F",
      "-R",
      "-S",
      "-X",
      "-K"
    )

    val pagerProcess = new ProcessBuilder(pagerCommand: _*)
      .redirectOutput(Redirect.INHERIT)
      .redirectError(Redirect.INHERIT)
      .start()
    val pager = new OutputStreamWriter(pagerProcess.getOutputStream())


    val final_ = lines.lines
    var curAuthorName: AuthorName = null
    var curCommitDate: CommitDate = null
    var previousAuthorName: AuthorName = null
    var previousCommitDate: CommitDate = null
    for ((columns, line_number) <- final_.view.zipWithIndex) {
        var line = ""
        var commitColor = ""
        var not_empty_line = false
        for ((column, idx) <- columns.view.zipWithIndex) {
          if (column != '|' && column != ' ') {
            not_empty_line = true
          }

          // unbold
          if (style(line_number)(idx)._1.startsWith("\u001b[1;")) {
            val codeStr = style(line_number)(idx)._1.replace("\u001b[1;", "").replace("m", "")
            val codeNumber = codeStr.toInt - 22
            style(line_number)(idx) = style(line_number)(idx).copy(
              _1 = "\u001b[38;5;" + codeNumber + "m")
          }

          if (column == '*' || column == '┬') {
            commitColor = style(line_number)(idx)._1.replace("\u001b[", "").replace("m", "")
            commitColor =
              if (commitColor.startsWith("38;5;")) commitColor.replace("38;5;", "")
              else if (commitColor.startsWith("1;")) (commitColor.replace("1;", "").toInt - 22).toString
              // TODO: Fix this bug
              else if (commitColor.isEmpty()) {
                // pager.write("PROBLEM" + commitColor + "\n")
                15.toString
              }
              else (commitColor.toInt - 30).toString
            commitColor = commitColor.takeWhile(_.isDigit)
          }

          if (column == '|') {
            final_(line_number)(idx) = '│'
          }

          if (column == '┬') {
            final_(line_number)(idx) = '╤'
          }

          if (column == '*') {
            final_(line_number)(idx) = '╪'
          }

          val finalColumn =
            if (config.selectedStyle == "thick-squared") {
              ThickSquaredStyle.apply(final_(line_number)(idx))
            } else {
              final_(line_number)(idx)
            }

          line = line + style(line_number)(idx)._1 + finalColumn + style(line_number)(idx)._2
        }


        val message = messages(line_number)

        val parsedMessage = gitLogGraph.parseMessage(message)

        var finalParsedMessage =
          if (commitColor.nonEmpty) {
            parsedMessage.flatMap {
              case h: Hash => Vector(h.copy(color = commitColor.toInt))
              case r: RefNames =>
                val refNames = r.copy(color = commitColor.toInt).withText(s" ${r.value} ")
                if (config.unicodeIcons) refNames.getBranches else Vector(refNames)
              case cd: CommitDate => curCommitDate = cd ; Vector(cd)
              case an: AuthorName => curAuthorName = an ; Vector(an)
              case other => Vector(other)
            }
          } else parsedMessage

        if (config.hideConsecutive && curCommitDate != null && curAuthorName == previousAuthorName) {
          val sameDate = curCommitDate == previousCommitDate
          finalParsedMessage = finalParsedMessage.map {
            case an: AuthorName => an.withText(" " * an.value.size)
            case cd: CommitDate if sameDate => cd.withText(" " * cd.value.size)
            case other => other
          }
        }

        previousCommitDate = curCommitDate
        previousAuthorName = curAuthorName

        val alignmentSpaces = if (config.alignCommitMessages) " " * (maxGraphLine - columns.size) else ""

        line = line + alignmentSpaces + finalParsedMessage.mkString(" ")

        if (config.verticalShrink == 0 || not_empty_line) {
          if (config.commitBulletIcon.nonEmpty) {
            val currentBulletIcon = if (config.selectedStyle == "thick-squared") ThickSquaredStyle.`╪`.toString else "╪"
            line = line.replace(currentBulletIcon, config.commitBulletIcon)
          }

          if (config.commitChildlessIcon.nonEmpty) {
            val currentChildlessIcon = if (config.selectedStyle == "thick-squared") ThickSquaredStyle.`╤`.toString else "╤"
            line = line.replace(currentChildlessIcon, config.commitBulletIcon)
          }

          if (config.unicodeIcons) {
            line = line.replace("{origin}", config.originIcon)
            line = line.replace("{HEAD}", config.headIcon)
            line = line.replace("{local}", config.localIcon)
            line = line.replace("{tag}", config.tagIcon)
          }

          pager.write(line + "\n")
          pager.flush()
        }
    }

    pager.close()
    pagerProcess.waitFor

    //# good_
    //# good chars for dot:
    //# ┿
    //# ╪
    //# ┯
    //# ╿
    //# ┃

  }

  def addColorToChildlessCommits(
      graphLines: mutable.ArrayBuffer[Array[Char]],
      style: mutable.ArrayBuffer[Array[(String, String)]],
      seed: Int) {
    val r = scala.util.Random

    var lidx = 0
    for (line <- graphLines) {
      var ridx = 0
      for (char <- line) {
        if (char == '*') {
          if (lidx - 1 < 0 ||
              ridx >= graphLines(lidx-1).size ||
              graphLines(lidx-1)(ridx) == ' ' ) {
            graphLines(lidx)(ridx) = '┬'
            r.setSeed(lidx + ridx + seed)
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