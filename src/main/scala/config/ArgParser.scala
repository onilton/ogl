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

    def helpText =
    """usage: ogl [options]
    |
    |-h, --help
    |          Prints this
    |
    |--debug
    |          Show debug messages
    |
    |-v, --version
    |          Print version
    |
    |--style=VALUE
    |          The graph style you want to display. Valid values: squared, dual, heavy, rounded (default)
    |
    |--unlimited-fields
    |          Do not limit the width of some fields. Note width values can be customized in ~/.ogl config
    |
    |--show-consecutive
    |          By default we hide consecutive dates and author, this option shows then even if they are repeated
    |
    |--no-align-messages
    |          Disable vertical alignment of commit messages
    |
    |--vertical-shrink=VALUE
    |          Try to shrink graph vertically. 0 is not shrink, 1 is simple shrink (default) and 2 is heavy shrink.
    |          Note that 2 may make ogl slower.
    |
    |--no-unicode-icons
    |          Do not show (pretty) unicode icons for ref/branches
    |
    |More config options, like colors, icons and others are available in config file ~/.ogl
    |Check configexample.ogl.
    |
    |ogl accepts most of git log commands, check man git-log for more.""".stripMargin

    private def getArgPresenceOption(arg: String) = if (args.contains(arg)) Some(true) else None


    private def getValueForArg(key: String) = {
      val arg = args.find(_.startsWith(key))

      arg.flatMap(_.split('=').drop(1).lastOption)
    }

    private val selectedStyle = getValueForArg("--style")

    private val verticalShrink = getValueForArg("--vertical-shrink").map(_.toInt)

    private val seed = getValueForArg("--vertical-shrink").map(_.toInt)

    private val maxAlignCommitMessages = getValueForArg("--max-align-messages").map(_.toInt)

    val partialConfig = PartialConfig(
      seed = seed,
      debugEnabled = getArgPresenceOption("--debug"),
      help = getArgPresenceOption("-h").orElse(getArgPresenceOption("--help")),
      version = getArgPresenceOption("-v").orElse(getArgPresenceOption("--version")),
      boldEnabled = getArgPresenceOption("--bold"),
      selectedStyle = selectedStyle.flatMap(GraphStyles.get),
      unlimitedFields = getArgPresenceOption("--unlimited-fields"),
      hideConsecutive = getArgPresenceOption("--show-consecutive").map(! _),
      alignCommitMessages = getArgPresenceOption("--no-align-messages").map(! _),
      maxAlignCommitMessages = maxAlignCommitMessages,
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
