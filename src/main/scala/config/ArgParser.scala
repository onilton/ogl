package config
import styles.GraphStyles

/* Poor's man implementation of a argument parser
 * scallop is not yet ported to scala-native
 * so this was the best I could do
 * */

 case class ArgParser(args: Array[String]) {
    // TO-DO:
    // --dynamic-paint-hash?? : paint hash with the same color as graph (Add underline to parse)
    // --dynamic-paint-branch?? : paint hash with the same color as graph
    // --compress-lines

    private def getArgPresenceOption(arg: String) = if (args.contains(arg)) Some(true) else None


    private def getValueForArg(key: String) = {
      val arg = args.find(_.startsWith(key))

      arg.flatMap(_.split('=').drop(1).lastOption)
    }

    private val selectedStyle = getValueForArg("--style")

    private val verticalShrink = getValueForArg("--vertical-shrink").map(_.toInt)

    private val seed = getValueForArg("--vertical-shrink").map(_.toInt)

    val partialConfig = PartialConfig(
      seed = seed,
      debugEnabled = getArgPresenceOption("--debug"),
      boldEnabled = getArgPresenceOption("--bold"),
      selectedStyle = selectedStyle.flatMap(GraphStyles.get),
      unlimitedFields = getArgPresenceOption("--unlimited-fields"),
      hideConsecutive = getArgPresenceOption("--show-consecutive").map(! _),
      alignCommitMessages = getArgPresenceOption("--no-align-messages").map(! _),
      verticalShrink = verticalShrink,
      unicodeIcons = getArgPresenceOption("--no-unicode-icons").map(! _)
    )

    val gitArgs = args
      .filterNot(_ == "--debug")
      .filterNot(_ == "--bold")
      .filterNot(arg => selectedStyle.exists(_ == arg))
      .filterNot(_ == "--show-consecutive")
      .filterNot(_ == "--unlimited-fields")
      .filterNot(_ == "--no-align-messages")
      .filterNot(_ == "--no-unicode-icons")
      .filterNot(_.startsWith("--vertical-shrink"))
      .filterNot(_.startsWith("--seed"))
      .filterNot(_.startsWith("--style"))
      .toSeq
}
