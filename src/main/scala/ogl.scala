import scala.collection.mutable
import commands.{ GitLogGraph, Hash, AuthorName, CommitDate, RefNames  }
import commands.Pager
import config.{ ArgParser, Config, ConfigFile }
import utils.AnsiEscapeCodes
import utils.Debugger
import utils.EasyMetrics._


object ogl {

  def main(args: Array[String]): Unit = {
    val argParser = ArgParser(args)
    val config = Config.getConfig(ConfigFile.getPartialConfig.toList ++ List(argParser.partialConfig))

    val d = Debugger(config.debugEnabled)

    d.debug("Started")
    startMeasurament()
    //import scala.io.Source
    //val data1 = Source.fromFile("um_tempcolor", "utf-8").getLines
    // Remember a line is broken(encoding)
    // better iterate in loop to avoid errors
    //val data1 = Source.fromFile("um_tempcolor", "utf-8").getLines.toArray

    if (config.help) {
      println(argParser.helpText)
      System.exit(0)
    }

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
    var graphLines: mutable.ArrayBuffer[Array[Char]] = new mutable.ArrayBuffer[Array[Char]](data1.size)
    var messages: mutable.ArrayBuffer[String] = new mutable.ArrayBuffer[String](data1.size)
    var maxGraphLine = 0

    for (rawLine <- data1) {
      val (escapes, line) = AnsiEscapeCodes.parseLine(rawLine)

      val index = line.indexWhere(c => !allGraphChars.contains(c))

      val (graph, message) =
        if (index >= 0) {
          val lastGraphIdx = index
          if (lastGraphIdx > maxGraphLine) {
            maxGraphLine = lastGraphIdx
          }
          line.splitAt(lastGraphIdx)
        } else {
          (line, "")
        }

      if (graph.exists(c => c == '/' || c == '\\')) {
        val commitCharIdx = graph.indexOf("*")
        var extended = graph
        if (commitCharIdx >= 0) {
          graphLines.lastOption.foreach { lastLine =>
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
        graphLines.append(extended.toArray)
        messages.append("")
      }

      graphLines.append(graph.replace("|_", "|─").toArray)
      style.append(escapes.toArray)
      messages.append(message)
    }

    d.debugNoNL("Summary | lines: " + graphLines.size)
    d.debug(" | original lines: " + data1.size)
    d.debug("Splitted graph " + took())
    startMeasurament()


    val lines = new GitGraphReplacer(graphLines.toArray, style)


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

    addColorToChildlessCommits(graphLines, style, config.seed)

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

    val pager = Pager()

    val final_ = lines.lines
    var curAuthorName: AuthorName = null
    var curCommitDate: CommitDate = null
    var previousAuthorName: AuthorName = null
    var previousCommitDate: CommitDate = null
    for ((columns, lineNumber) <- final_.view.zipWithIndex) {
        var line = ""
        var commitColor = ""
        var notEmptyLine = false
        for ((column, idx) <- columns.view.zipWithIndex) {
          if (column != '|' && column != ' ') {
            notEmptyLine = true
          }

          // unbold
          if (style(lineNumber)(idx)._1.startsWith("\u001b[1;")) {
            val codeStr = style(lineNumber)(idx)._1.replace("\u001b[1;", "").replace("m", "")
            val codeNumber = codeStr.toInt - 22
            style(lineNumber)(idx) = style(lineNumber)(idx).copy(
              _1 = "\u001b[38;5;" + codeNumber + "m")
          }

          if (column == '*' || column == '┬') {
            commitColor = style(lineNumber)(idx)._1.replace("\u001b[", "").replace("m", "")
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
            final_(lineNumber)(idx) = '│'
          }

          if (column == '┬') {
            final_(lineNumber)(idx) = '┯'
          }

          if (column == '*') {
            final_(lineNumber)(idx) = '┿'
          }

          val finalColumn =
            if (config.selectedStyle == styles.Default) {
              final_(lineNumber)(idx)
            } else {
              config.selectedStyle.apply(final_(lineNumber)(idx))
            }

          line = line + style(lineNumber)(idx)._1 + finalColumn + style(lineNumber)(idx)._2
        }


        val message = messages(lineNumber)

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

        if (config.verticalShrink == 0 || notEmptyLine) {
          if (config.commitBulletIcon.nonEmpty) {
            val currentBulletIcon = config.selectedStyle.`┿`.toString
            line = line.replace(currentBulletIcon, config.commitBulletIcon)
          }

          if (config.commitChildlessIcon.nonEmpty) {
            val currentChildlessIcon = config.selectedStyle.`┯`.toString
            line = line.replace(currentChildlessIcon, config.commitBulletIcon)
          }

          if (config.unicodeIcons) {
            line = line.replace("{origin}", config.originIcon)
            line = line.replace("{HEAD}", config.headIcon)
            line = line.replace("{local}", config.localIcon)
            line = line.replace("{tag}", config.tagIcon)
          }

          pager.println(line)
        }
    }

    pager.waitFor
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
}