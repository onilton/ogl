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