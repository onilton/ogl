package utils

case class Debugger(enabled: Boolean) {
    def debug(s: String): Unit = {
      if (enabled)
        println(s)
    }

    def debugNoNL(s: String): Unit = {
      if (enabled)
        print(s)
    }
}