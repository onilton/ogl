package commands

import java.io.OutputStreamWriter
import java.lang.ProcessBuilder.Redirect

case class Pager() {
  val pagerCommand = Seq(
    "less",
    "-F", // --quit-if-one-screen - automatically exit if the entire file
          //                        can be displayed on the first screen
    "-R", // --RAW-CONTROL-CHARS
    "-S", // --chop-long-lines - Causes lines longer than the screen width to
          //                     be chopped (truncated) rather than wrapped.
    "-X", // --no-init - Disables sending the termcap initialization and
          //             deinitialization strings to the terminal.
    "-K"  // --quit-on-intr - Causes less to exit immediately (with status 2)
          //                  when an interrupt character (usually ^C) is
          //                  typed.
  )

  val pagerProcess = new ProcessBuilder(pagerCommand: _*)
    .redirectOutput(Redirect.INHERIT)
    .redirectError(Redirect.INHERIT)
    .start()
  val outputStreamWriter = new OutputStreamWriter(pagerProcess.getOutputStream())

  def println(line: String) = {
    outputStreamWriter.write(line + "\n")
    outputStreamWriter.flush()
  }

  def waitFor: Unit = {
    outputStreamWriter.close()
    pagerProcess.waitFor
  }
}