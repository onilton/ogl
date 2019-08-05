package config

import scala.io.Source
import java.io.File
import styles.GraphStyle

/* Poor's man implementation of a HOCON config
 * sconfig is not yet ported to scala-native https://github.com/ekrich/sconfig/issues/33
 * so this was the best I could do
 * */
case class Config(
  seed: Int,
  debugEnabled: Boolean,
  help: Boolean,
  version: Boolean,
  boldEnabled: Boolean,
  selectedStyle: GraphStyle,
  unlimitedFields: Boolean,
  hideConsecutive: Boolean,
  alignCommitMessages: Boolean,
  maxAlignCommitMessages: Int,
  verticalShrink: Int,
  subjectWidth: Int,
  authorNameWidth: Int,
  commitDateWidth: Int,
  subjectColor: Int,
  authorNameColor: Int,
  commitDateColor: Int,
  unicodeIcons: Boolean,
  originIcon: String,
  headIcon: String,
  localIcon: String,
  tagIcon: String,
  commitBulletIcon: String,
  commitChildlessIcon: String
)

case class PartialConfig(
  seed: Option[Int] = None,
  debugEnabled: Option[Boolean] = None,
  help: Option[Boolean] = None,
  version: Option[Boolean] = None,
  boldEnabled: Option[Boolean] = None,
  selectedStyle: Option[GraphStyle] = None,
  unlimitedFields: Option[Boolean] = None,
  hideConsecutive: Option[Boolean] = None,
  alignCommitMessages: Option[Boolean] = None,
  maxAlignCommitMessages: Option[Int] = None,
  verticalShrink: Option[Int] = None,
  subjectWidth: Option[Int] = None,
  authorNameWidth: Option[Int] = None,
  commitDateWidth: Option[Int] = None,
  subjectColor: Option[Int] = None,
  authorNameColor: Option[Int] = None,
  commitDateColor: Option[Int] = None,
  unicodeIcons: Option[Boolean] = None,
  originIcon: Option[String] = None,
  headIcon: Option[String] = None,
  localIcon: Option[String] = None,
  tagIcon: Option[String] = None,
  commitBulletIcon: Option[String] = None,
  commitChildlessIcon: Option[String] = None
)

object Config {
  val default = Config(
    seed = 3,
    debugEnabled = false,
    help = false,
    version = false,
    boldEnabled = false,
    selectedStyle = styles.Default,
    unlimitedFields = false,
    hideConsecutive = true,
    alignCommitMessages = true,
    maxAlignCommitMessages = 70,
    verticalShrink = 1,
    subjectWidth = 60,
    authorNameWidth = 15,
    commitDateWidth = 15,
    subjectColor = 15,
    authorNameColor = 66,
    commitDateColor = 237,
    unicodeIcons = true,
    originIcon = "📡 ",
    headIcon = "✓",
    localIcon = "💻 ",
    tagIcon = "🎫 ",
    commitBulletIcon = "",
    commitChildlessIcon = ""
  )

  def getConfig(partialCfgs: List[PartialConfig]) = {
    Config(
      seed = partialCfgs.flatMap(_.seed).lastOption.getOrElse(default.seed),
      debugEnabled = partialCfgs.flatMap(_.debugEnabled).lastOption.getOrElse(default.debugEnabled),
      help = partialCfgs.flatMap(_.help).lastOption.getOrElse(default.help),
      version = partialCfgs.flatMap(_.version).lastOption.getOrElse(default.version),
      boldEnabled = partialCfgs.flatMap(_.boldEnabled).lastOption.getOrElse(default.boldEnabled),
      selectedStyle = partialCfgs.flatMap(_.selectedStyle).lastOption.getOrElse(default.selectedStyle),
      unlimitedFields = partialCfgs.flatMap(_.unlimitedFields).lastOption.getOrElse(default.unlimitedFields),
      hideConsecutive = partialCfgs.flatMap(_.hideConsecutive).lastOption.getOrElse(default.hideConsecutive),
      alignCommitMessages = partialCfgs.flatMap(_.alignCommitMessages).lastOption.getOrElse(default.alignCommitMessages),
      maxAlignCommitMessages = partialCfgs.flatMap(_.maxAlignCommitMessages).lastOption.getOrElse(default.maxAlignCommitMessages),
      unicodeIcons = partialCfgs.flatMap(_.unicodeIcons).lastOption.getOrElse(default.unicodeIcons),
      verticalShrink = partialCfgs.flatMap(_.verticalShrink).lastOption.getOrElse(default.verticalShrink),
      subjectWidth = partialCfgs.flatMap(_.subjectWidth).lastOption.getOrElse(default.subjectWidth),
      authorNameWidth = partialCfgs.flatMap(_.authorNameWidth).lastOption.getOrElse(default.authorNameWidth),
      commitDateWidth = partialCfgs.flatMap(_.commitDateWidth).lastOption.getOrElse(default.commitDateWidth),
      subjectColor = partialCfgs.flatMap(_.subjectColor).lastOption.getOrElse(default.subjectColor),
      authorNameColor = partialCfgs.flatMap(_.authorNameColor).lastOption.getOrElse(default.authorNameColor),
      commitDateColor = partialCfgs.flatMap(_.commitDateColor).lastOption.getOrElse(default.commitDateColor),
      originIcon = partialCfgs.flatMap(_.originIcon).lastOption.getOrElse(default.originIcon),
      headIcon = partialCfgs.flatMap(_.headIcon).lastOption.getOrElse(default.headIcon),
      localIcon = partialCfgs.flatMap(_.localIcon).lastOption.getOrElse(default.localIcon),
      tagIcon = partialCfgs.flatMap(_.tagIcon).lastOption.getOrElse(default.tagIcon),
      commitBulletIcon = partialCfgs.flatMap(_.commitBulletIcon).lastOption.getOrElse(default.commitBulletIcon),
      commitChildlessIcon = partialCfgs.flatMap(_.commitChildlessIcon).lastOption.getOrElse(default.commitChildlessIcon)
    )
  }

}
