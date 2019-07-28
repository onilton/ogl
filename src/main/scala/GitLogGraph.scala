import scala.io.Codec
import java.nio.charset.CodingErrorAction
import scala.io.Source

abstract class Placeholder(val value: String)

object Placeholders {
  object Separator extends Placeholder("-")
  object Hash extends Placeholder("%h")
  object Subject extends Placeholder("%s")
  object AuthorName extends Placeholder("[%an]")
  object CommitDate extends Placeholder("(%cr)")
  object RefNames extends Placeholder("%D")
}


object AnsiEscapesCodes {
  def ansiEscape(value: String): String = s"\u001b[" + value +"m"
  def ansiEscape(value: Int): String = ansiEscape(value.toString())
  def color(color: String) = ansiEscape(s"38;5;${color}")
  def color(color: Int) = ansiEscape(s"38;5;${color}")
  def invert = ansiEscape(7)
  def underline = ansiEscape(4)
  def reset = ansiEscape(0)
}

trait CommitInfo {
  def prefix = ""
  def sufix = ""
  def color: Int
  def inverted: Boolean = false
  def underlined: Boolean = false
  def placeholder: Placeholder
  def value: String = ""
  def withText(text: String): CommitInfo

  private val ansi = AnsiEscapesCodes

  private def invertEscape = if (inverted) ansi.invert else ""
  private def underlineEscape = if (underlined) ansi.underline else ""

  private def colorEscape = ansi.color(color)

  override def toString(): String = {
    underlineEscape + invertEscape + colorEscape + value + ansi.reset
  }
}


case class Separator(color: Int = 15, placeholder: Placeholder = Placeholders.Separator, override val value: String = "") extends CommitInfo {
  def withText(text: String): Separator = copy(value=text)
}
case class Hash(color: Int = 1, placeholder: Placeholder = Placeholders.Hash, override val value: String = "") extends CommitInfo {
  def withText(text: String): Hash = copy(value=text)
}
case class Subject(color: Int = 15, placeholder: Placeholder = Placeholders.Subject, override val value: String = "") extends CommitInfo {
  def withText(text: String): Subject = copy(value=text)
}
case class AuthorName(color: Int = 66, placeholder: Placeholder = Placeholders.AuthorName, override val value: String = "") extends CommitInfo {
  def withText(text: String): AuthorName = copy(value=text)
}
case class CommitDate(color: Int = 237, placeholder: Placeholder = Placeholders.CommitDate, override val value: String = "") extends CommitInfo {
  def withText(text: String): CommitDate = copy(value=text)
}
case class RefNames(color: Int = 3, placeholder: Placeholder = Placeholders.RefNames, override val value: String = "", override val inverted: Boolean = true) extends CommitInfo {
  def withText(text: String): RefNames = copy(value=text)

  def getBranches = {
    val branches = value.split(',').map { b =>
      var tempB = (b.trim, Vector.empty[String])
      if (tempB._1.contains("HEAD -> ")) tempB = (tempB._1.replace("HEAD -> ", "").trim, tempB._2 :+ "{HEAD}")

      if (tempB._1.contains("origin/")) tempB = (tempB._1.replace("origin/", "").trim, tempB._2 :+ "{origin}")
      else if (tempB._1.contains("tag:")) tempB = (tempB._1.replace("tag:", "").trim, tempB._2 :+ "{tag}")
      else tempB = (tempB._1, tempB._2 :+ "{local}")

      tempB
    }.groupBy(_._1).mapValues(_.flatMap(_._2))

    branches.map { case (bName, bNotes)  =>
      val (prefix, name, sufix) =
        if (bNotes.contains("{HEAD}")) {
          ("{HEAD} ", bName, bNotes.filterNot( _ == "{HEAD}"))
        } else {
          ("", bName, bNotes)
        }
      withText(" " + prefix + bName + " " + sufix.mkString("") + " ")
    }
  }
}

object GitLogGraph {
  private val p = Placeholders

  val defaultFormat: Vector[CommitInfo] = Vector(
    Hash(),
    Separator(),
    Subject(),
    AuthorName(),
    CommitDate(),
    RefNames()
  )
}

case class GitLogGraph(private val format: Vector[CommitInfo], args: Seq[String]) {
  val gitCommand = Seq(
    "git",
    "log",
    "--graph",
    //"--pretty=\"format:\\%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset\"",
    //"--pretty=format:%Cred%h%Creset - %s %Cgreen(%cr) %C(bold blue)<%an>%Creset \u001b[7m%C(yellow)%D%Creset",
    //"--pretty=format:\u001b[4m%Cred%h%Creset - %s \u001b[38;5;66m[%an]%Creset \u001b[38;5;237m(%cr)%Creset \u001b[4m\u001b[7m%C(yellow)% D%Creset",
    //  "--pretty=format:\u001b[4m%Cred%h%Creset - %s \u001b[38;5;66m[%an]%Creset \u001b[38;5;237m(%cr)%Creset \u001b[4m\u001b[7m%C(yellow)% D%Creset",
    "--pretty=format:" + format.map(_.placeholder.value).mkString("\u0008"),
    //"--pretty=format:%h -%d %s (%cr) <%an>",
    "--abbrev-commit",
    "--color"
  ) ++ args

  private val proc = new ProcessBuilder(gitCommand: _*).start()
  private implicit val codec = Codec("UTF-8")
  codec.onMalformedInput(CodingErrorAction.REPLACE)
  codec.onUnmappableCharacter(CodingErrorAction.REPLACE)
  val out = Source.fromInputStream(proc.getInputStream).getLines.toArray
  proc.waitFor

  val msgFormat = format

  def parseMessage(msg: String) = {
    val parts = msg.split('\u0008')
    //require(parts.size == msgFormat.size, "Text and format size doesn't match")
    parts.view.zip(msgFormat).map { case (part, formatItem) => formatItem.withText(part) }.toVector
  }

}