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

    private val selectedStyle = if (args.contains("--style")) {
      val idx = args.indexOf("--style")
        Option(args(idx + 1))
      } else {
        None
      }

    val partialConfig = PartialConfig(
      debugEnabled = getArgPresenceOption("--debug"),
      boldEnabled = getArgPresenceOption("--bold"),
      selectedStyle = selectedStyle,
      unlimitedFields = getArgPresenceOption("--unlimited-fields"),
      hideConsecutive = getArgPresenceOption("--show-consecutive").map(! _),
      alignCommitMessages = getArgPresenceOption("--no-align-messages").map(! _),
      unicodeIcons = getArgPresenceOption("--no-unicode-icons").map(! _)
    )

    val gitArgs = args
      .filterNot(_ == "--debug")
      .filterNot(_ == "--bold")
      .filterNot(_ == "--style")
      .filterNot(_ == selectedStyle)
      .filterNot(_ == "--show-consecutive")
      .filterNot(_ == "--unlimited-fields")
      .filterNot(_ == "--no-align-messages")
      .filterNot(_ == "--no-unicode-icons")
      .toSeq
 }