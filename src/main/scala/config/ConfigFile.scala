package config

import scala.io.Source
import java.io.File
import scala.util.Try
import java.nio.file.Path
import java.nio.file.Paths
import styles.GraphStyles

/* Poor's man implementation of a HOCON config
 * sconfig is not yet ported to scala-native https://github.com/ekrich/sconfig/issues/33
 * so this was the best I could do
 * */
case class ConfigFile(file: File) {
  private val lines = Source.fromFile(file, "utf-8")
    .getLines
    .map(_.trim())
    .filterNot(l => l.isEmpty() || l.startsWith("#") || l.startsWith("//"))
    .filter(_.contains("="))
    .map(Entry.fromLine)
    .toVector


  private def getValue(key: String) = lines.find(_.key == key).map(_.value)

  private def getBoolean(key: String) = getValue(key).map(_.toBoolean)

  private def getInt(key: String) = getValue(key).map(_.toInt)

  val partialConfig = PartialConfig(
    seed = getInt("seed"),
    selectedStyle = getValue("style").flatMap(GraphStyles.get),
    unlimitedFields = getBoolean("unlimited-fields"),
    hideConsecutive =  getBoolean("hide-consecutive"),
    alignCommitMessages = getBoolean("align-messages"),
    verticalShrink = getInt("vertical-shrink"),
    subjectWidth = getInt("subject.width"),
    authorNameWidth = getInt("author.width"),
    commitDateWidth = getInt("date.width"),
    subjectColor = getInt("subject.color"),
    authorNameColor = getInt("author.color"),
    commitDateColor = getInt("date.color"),
    unicodeIcons = getBoolean("unicode-icons"),
    originIcon = getValue("icon.origin"),
    headIcon = getValue("icon.head"),
    localIcon = getValue("icon.local"),
    tagIcon = getValue("icon.tag"),
    commitBulletIcon = getValue("icon.commit.bullet"),
    commitChildlessIcon = getValue("icon.commit.childless")
  )
}

object ConfigFile {
  val configFile = Paths.get(System.getProperty("user.home"), ".ogl").toFile()

  def getPartialConfig() =
    if (configFile.exists()) {
      Some(ConfigFile(configFile).partialConfig)
    } else {
      None
    }
}

case class Entry(key: String, value: String)

object Entry {
  def fromLine(line: String) = {
    val Array(key, value) = line.split("=").map(_.trim())

    val cleanValue = value.dropWhile(c => c == '"').takeWhile(c => c != '"')
    Entry(key, cleanValue)
  }
}