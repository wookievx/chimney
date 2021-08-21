package io.scalaland.chimney.examples

object colors1 {
  sealed trait Color
  case object Green extends Color
  case object Blue extends Color
  case object Red extends Color

}

object colors2 {
  sealed trait Color
  case object Green extends Color
  case object Blue extends Color
  case object Red extends Color
  case object Black extends Color
}
