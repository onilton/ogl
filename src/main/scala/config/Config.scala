package config

import scala.io.Source
import java.io.File

/* Poor's man implementation of a HOCON config
 * sconfig is not yet ported to scala-native https://github.com/ekrich/sconfig/issues/33
 * so this was the best I could do
 * */
case class Config(
  debugEnabled: Boolean,
  boldEnabled: Boolean,
  selectedStyle: String,
  unlimitedFields: Boolean,
  hideConsecutive: Boolean,
  alignCommitMessages: Boolean,
  unicodeIcons: Boolean
)

case class PartialConfig(
  debugEnabled: Option[Boolean] = None,
  boldEnabled: Option[Boolean] = None,
  selectedStyle: Option[String] = None,
  unlimitedFields: Option[Boolean] = None,
  hideConsecutive: Option[Boolean] = None,
  alignCommitMessages: Option[Boolean] = None,
  unicodeIcons: Option[Boolean] = None
)

object Config {
  val default = Config(
    debugEnabled = false,
    boldEnabled = false,
    selectedStyle = "rounded",
    unlimitedFields = false,
    hideConsecutive = true,
    alignCommitMessages = true,
    unicodeIcons = true
  )

  def getConfig(partialCfgs: List[PartialConfig]) = {
    Config(
      debugEnabled = partialCfgs.flatMap(_.debugEnabled).lastOption.getOrElse(default.debugEnabled),
      boldEnabled = partialCfgs.flatMap(_.boldEnabled).lastOption.getOrElse(default.boldEnabled),
      selectedStyle = partialCfgs.flatMap(_.selectedStyle).lastOption.getOrElse(default.selectedStyle),
      unlimitedFields = partialCfgs.flatMap(_.unlimitedFields).lastOption.getOrElse(default.unlimitedFields),
      hideConsecutive = partialCfgs.flatMap(_.hideConsecutive).lastOption.getOrElse(default.hideConsecutive),
      alignCommitMessages = partialCfgs.flatMap(_.alignCommitMessages).lastOption.getOrElse(default.alignCommitMessages),
      unicodeIcons = partialCfgs.flatMap(_.unicodeIcons).lastOption.getOrElse(default.unicodeIcons)
    )
  }

}
