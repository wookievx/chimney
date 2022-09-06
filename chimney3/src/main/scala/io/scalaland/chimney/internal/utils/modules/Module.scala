package io.scalaland.chimney.internal.utils.modules

import scala.quoted.*

private[internal] trait Module:
  val quotes: Quotes

  given Quotes = quotes

  import quotes.reflect.*

  given Printer[TypeRepr] = Printer.TypeReprShortCode
  given Printer[Tree] = Printer.TreeShortCode

end Module
