package config

import scala.io.Source
import java.io.File

/* Poor's man implementation of a HOCON config
 * sconfig is not yet ported to scala-native https://github.com/ekrich/sconfig/issues/33
 * so this was the best I could do
 * */
case class Config(
  debugEnabled: Boolean = false,
  boldEnabled: Boolean = false,
  selectedStyle: String = "rounded",
  unlimitedFields: Boolean = false,
  hideConsecutive: Boolean = true,
  alignCommitMessages: Boolean = true
)

object Config {
  val default = Config()

  def getConfig(debugEnabled: Option[Boolean],
                boldEnabled: Option[Boolean],
                selectedStyle: Option[String],
                unlimitedFields: Option[Boolean],
                hideConsecutive: Option[Boolean],
                alignCommitMessages: Option[Boolean]) = {
    Config(
      debugEnabled = debugEnabled.getOrElse(default.debugEnabled),
      boldEnabled = boldEnabled.getOrElse(default.boldEnabled),
      selectedStyle = selectedStyle.getOrElse(default.selectedStyle),
      unlimitedFields = unlimitedFields.getOrElse(default.unlimitedFields),
      hideConsecutive = hideConsecutive.getOrElse(default.hideConsecutive),
      alignCommitMessages = alignCommitMessages.getOrElse(default.alignCommitMessages)
    )
  }

}
