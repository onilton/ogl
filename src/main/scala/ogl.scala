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
    println("Started")
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
    ) ++ args.toSeq

    val proc = new ProcessBuilder(gitCommand: _*).start()
    implicit val codec = Codec("UTF-8")
    codec.onMalformedInput(CodingErrorAction.REPLACE)
    codec.onUnmappableCharacter(CodingErrorAction.REPLACE)
    val out  = Source.fromInputStream(proc.getInputStream).getLines.toArray
    proc.waitFor
    val data1 = out
    //val data1 = out.mkString.split("\n")
    println("ok")





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
    var message = ""
    var idx = columns.size
    for (c <- messages(line_number).toArray) {
      message = message + style(line_number)(idx)._1 + c + style(line_number)(idx)._2
      idx += 1
    }

    line = line + message

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

  }

  def parse_line(line: String): (Array[(String, String)], String) = {
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