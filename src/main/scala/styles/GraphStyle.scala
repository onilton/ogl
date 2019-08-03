package styles

trait GraphStyle {
  def `│`: Char
  def `╭`: Char
  def `╰`: Char
  def `╮`: Char
  def `╯`: Char
  def `─`: Char
  def `╪`: Char
  def `├`: Char
  def `╤`: Char
  def empty: Char
  def unknown: Char

  def apply(column: Char) = {
    if (column == '│') `│`
    else if (column == '╭') `╭`
    else if (column == '╰') `╰`
    else if (column == '╮') `╮`
    else if (column == '╯') `╯`
    else if (column == '─') `─`
    else if (column == '╪') `╪`
    else if (column == '├') `├`
    else if (column == '╤') `╤`
    else if (column == ' ') empty
    else unknown
  }
}

object GraphStyles {
  def get(str: String): Option[GraphStyle] = str match {
    case "squared" => Some(Squared)
    case "dual" => Some(Dual)
    case "heavy-squared" => Some(HeavySquared)
    case "default" | "rounded" => Some(Default)
    case _ => None
  }

}