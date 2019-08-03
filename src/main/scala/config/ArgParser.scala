package config

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

    private val selectedStyleArg = args.find(_.startsWith("--style"))
    private val selectedStyle = selectedStyleArg.flatMap(_.split('=').drop(1).lastOption)

    private val verticalShrinkArg = args.find(_.startsWith("--vertical-shrink"))
    private val verticalShrink = verticalShrinkArg.flatMap(_.split('=').drop(1).headOption.map(_.toInt))

    private val seedArg = args.find(_.startsWith("--seed"))
    private val seed = seedArg.flatMap(_.split('=').drop(1).headOption.map(_.toInt))

    val partialConfig = PartialConfig(
      seed = seed,
      debugEnabled = getArgPresenceOption("--debug"),
      boldEnabled = getArgPresenceOption("--bold"),
      selectedStyle = selectedStyle,
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
