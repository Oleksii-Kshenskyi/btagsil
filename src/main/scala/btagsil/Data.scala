package btagsil

object Text {
    def exitMessage(): String = "Thanks for playing! See you soon!\n"

    def echoed(what: String): String = "Echoed => '" + what + "';\n"

    def unknownInput(wtf: String): String = "Wait... What is " + wtf + "?\n"
}
