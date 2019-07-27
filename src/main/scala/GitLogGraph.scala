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
  //def placeholder: String
  def value: Either[Placeholder, String]
  def withText(text: String): CommitInfo

  private val ansi = AnsiEscapesCodes

  private def invertEscape = if (inverted) ansi.invert else ""
  private def underlineEscape = if (underlined) ansi.underline else ""

  private def colorEscape = ansi.color(color)

  override def toString(): String = {
    val finalValue = value match {
      case Right(text) => text
      case Left(placeholder) => placeholder.value
    }
    underlineEscape + invertEscape + colorEscape + finalValue + ansi.reset
    //finalValue + "\u0000"
  }
}


case class Separator(color: Int = 15, value: Either[Placeholder, String] = Left(Placeholders.Separator)) extends CommitInfo {
  def withText(text: String): Separator = copy(value=Right(text))
}
case class Hash(color: Int = 1,  value: Either[Placeholder,String] = Left(Placeholders.Hash)) extends CommitInfo {
  def withText(text: String): Hash = copy(value=Right(text))
}
case class Subject(color: Int = 15, value: Either[Placeholder,String] = Left(Placeholders.Subject)) extends CommitInfo {
  def withText(text: String): Subject = copy(value=Right(text))
}
case class AuthorName(color: Int = 66, value: Either[Placeholder,String] = Left(Placeholders.AuthorName)) extends CommitInfo {
  def withText(text: String): AuthorName = copy(value=Right(text))
}
case class CommitDate(color: Int = 237, value: Either[Placeholder,String] = Left(Placeholders.CommitDate)) extends CommitInfo {
  def withText(text: String): CommitDate = copy(value=Right(text))
}
case class RefNames(color: Int = 3, value: Either[Placeholder,String] = Left(Placeholders.RefNames), override val inverted: Boolean = true) extends CommitInfo {
  def withText(text: String): RefNames = copy(value=Right(text))
}

object GitLogGraph {
  private val p = Placeholders

  val defaultFormat: List[Placeholder] = List(
    p.Hash,
    p.Separator,
    p.Subject,
    p.AuthorName,
    p.CommitDate,
    p.RefNames
  )

  val infos: Vector[CommitInfo] = Vector(
    Hash(),
    Separator(),
    Subject(),
    AuthorName(),
    CommitDate(),
    RefNames()
  )
}

case class GitLogGraph(private val format: List[Placeholder], args: Seq[String]) {
  val gitCommand = Seq(
    "git",
    "log",
    "--graph",
    //"--pretty=\"format:\\%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset\"",
    //"--pretty=format:%Cred%h%Creset - %s %Cgreen(%cr) %C(bold blue)<%an>%Creset \u001b[7m%C(yellow)%D%Creset",
    //"--pretty=format:\u001b[4m%Cred%h%Creset - %s \u001b[38;5;66m[%an]%Creset \u001b[38;5;237m(%cr)%Creset \u001b[4m\u001b[7m%C(yellow)% D%Creset",
    //  "--pretty=format:\u001b[4m%Cred%h%Creset - %s \u001b[38;5;66m[%an]%Creset \u001b[38;5;237m(%cr)%Creset \u001b[4m\u001b[7m%C(yellow)% D%Creset",
    "--pretty=format:" + format.map(_.value).mkString("|"),
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

  val msgFormat = GitLogGraph.infos

  def parseMessage(msg: String) = {
    val parts = msg.split('|')
    require(parts.size == msgFormat.size, "Text and format size doesn't match")
    parts.view.zip(msgFormat).map { case (part, formatItem) => formatItem.withText(part) }.toVector
  }

}