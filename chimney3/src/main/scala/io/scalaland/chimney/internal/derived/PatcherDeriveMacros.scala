package io.scalaland.chimney.internal.derived

import io.scalaland.chimney.Patcher
import io.scalaland.chimney.internal.utils.modules.*

import scala.deriving.*
import scala.quoted.*

class PatcherDeriveMacros(val quotes: Quotes)
    extends ProductDerivePatcherMacros
    with IterablePatcherDeriveMacros
    with FieldModule
    with MirrorModule
    with Module:
  import quotes.reflect.*

  def derive[A: Type, B: Type, Config <: Tuple: Type](
    path: Option[String]
  ): Expr[Patcher[A, B]] =
    def productResult: Option[Expr[Patcher[A, B]]] =
      (Expr.summon[Mirror.ProductOf[A]], Expr.summon[Mirror.ProductOf[B]]) match
        case (Some(mirrorA), Some(mirrorB)) =>
          val productMirrorA = ProductMirror.fromMirror(mirrorA)
          val productMirrorB = ProductMirror.fromMirror(mirrorB)
          Some(
            deriveProduct[A, B, Config](
              path.getOrElse(""),
              productMirrorA,
              productMirrorB
            )
          )
        case _ =>
          None
    end productResult

    productResult orElse
      deriveMap[A, B, Config](path.getOrElse("")) orElse
      deriveIterable[A, B, Config](path.getOrElse("")) getOrElse
      report.errorAndAbort(
        s"Requested patcher combination not supported, at: $path"
      )
  end derive
end PatcherDeriveMacros

object PatcherDeriveMacros:

  def derive[A: Type, B: Type, Config <: Tuple: Type](using
    Quotes
  ): Expr[Patcher[A, B]] =
    PatcherDeriveMacros(quotes).derive[A, B, Config](None)

end PatcherDeriveMacros
