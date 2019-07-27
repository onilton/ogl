import scala.io.Codec
import java.nio.charset.CodingErrorAction
import scala.io.Source


trait CommitInfo {
  def prefix = ""
  def sufix = ""
  def color: Int
  def inverted: Boolean = false
  def underlined: Boolean = false
  def placeholder: String

  private def ansiEscape(value: String): String = s"\u001b[" + value +"m"
  private def ansiEscape(value: Int): String = ansiEscape(value.toString())

  private def invertEscape = if (inverted) ansiEscape(7) else ""
  private def underlineEscape = if (underlined) ansiEscape(4) else ""

  private def colorEscape = ansiEscape(s"38;5;${color}")
  private def reset = ansiEscape(0)

  override def toString(): String = {
    underlineEscape + invertEscape + colorEscape + placeholder + reset
  }
}

case class Separator(color: Int = 7, placeholder: String = "-") extends CommitInfo
case class Hash(color: Int = 1, placeholder: String = "%h", override val underlined: Boolean = true) extends CommitInfo
case class Subject(color: Int = 15, placeholder: String = "%s") extends CommitInfo
case class AuthorName(color: Int = 66, placeholder: String = "[%an]") extends CommitInfo
case class CommitDate(color: Int = 237, placeholder: String = "(%cr)") extends CommitInfo
case class RefNames(color: Int = 3, placeholder: String = "%D", override val inverted: Boolean = true,
  override val underlined: Boolean = true) extends CommitInfo

object GitLogGraph {
  val defaultFormat: List[CommitInfo] = List(
    Hash(),
    Separator(),
    Subject(),
    AuthorName(),
    CommitDate(),
    RefNames()
  )
}

case class GitLogGraph(private val format: List[CommitInfo], args: Seq[String]) {
  val gitCommand = Seq(
    "git",
    "log",
    "--graph",
    //"--pretty=\"format:\\%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset\"",
    //"--pretty=format:%Cred%h%Creset - %s %Cgreen(%cr) %C(bold blue)<%an>%Creset \u001b[7m%C(yellow)%D%Creset",
    //"--pretty=format:\u001b[4m%Cred%h%Creset - %s \u001b[38;5;66m[%an]%Creset \u001b[38;5;237m(%cr)%Creset \u001b[4m\u001b[7m%C(yellow)% D%Creset",
    //  "--pretty=format:\u001b[4m%Cred%h%Creset - %s \u001b[38;5;66m[%an]%Creset \u001b[38;5;237m(%cr)%Creset \u001b[4m\u001b[7m%C(yellow)% D%Creset",
    "--pretty=format:" + format.map(_.toString()).mkString(" "),
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



}