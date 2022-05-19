package lib.conversions

import lib.syntax.StringSyntax.{_, given}


object StringConversion:

    given StringToBinary: Conversion[String, Seq[Boolean]] with
        def apply(string: String): Seq[Boolean] = string.binary
