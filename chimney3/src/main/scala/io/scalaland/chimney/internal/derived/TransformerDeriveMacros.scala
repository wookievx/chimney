package io.scalaland.chimney.internal.derived

import io.scalaland.chimney.dsl.{TransformerDefinition, TransformerFDefinition}
import io.scalaland.chimney.internal.TransformerFlag
import io.scalaland.chimney.internal.utils.modules.*
import io.scalaland.chimney.*

import scala.deriving.*
import scala.quoted.*

class TransformerDeriveMacros(val quotes: Quotes)
    extends ProductDeriveMacros
    with CoproductDeriveMacros
    with CollectionDeriveModule
    with CommonDeriveModule
    with FieldModule
    with MirrorModule
    with ConfigModule
    with Module:
  import quotes.reflect.*

  def deriveTransformerMacro[
    A: Type,
    B: Type,
    Conf <: Tuple: Type,
    Flags <: Tuple: Type
  ](
    source: Expr[A],
    definition: Expr[TransformerDefinition[A, B, Conf, Flags]]
  ): Expr[B] =
    val materialized = TransformerDefinitionMaterialized.materialize(definition)
    deriveTransformer[A, B, Flags](source, materialized)
  end deriveTransformerMacro

  private[internal] def deriveTransformer[
    A: Type,
    B: Type,
    Flags <: Tuple: Type
  ](
    source: Expr[A],
    materialized: TransformerDefinitionMaterialized[Flags]
  ): Expr[B] =
    val productResult: Option[Expr[B]] =
      (Expr.summon[Mirror.ProductOf[A]], Expr.summon[Mirror.ProductOf[B]]) match
        case (Some(mirrorA), Some(mirrorB)) =>
          val productMirrorA = ProductMirror.fromMirror(mirrorA)
          val productMirrorB = ProductMirror.fromMirror(mirrorB)
          Some(
            deriveProduct[A, B, Flags](
              source,
              materialized,
              productMirrorA,
              productMirrorB
            )
          )
        case (None, Some(mirrorB)) =>
          if materialized.hasAFlag[TransformerFlag.BeanGetters] then
            val productMirrorB = ProductMirror.fromMirror(mirrorB)
            Some(
              deriveProductFromJavaBean[A, B, Flags](
                source,
                materialized,
                productMirrorB
              )
            )
          else
            None
          end if
        case _ =>
          None

    val coproductResult: Option[Expr[B]] =
      (Expr.summon[Mirror.SumOf[A]], Expr.summon[Mirror.SumOf[B]]) match
        case (Some(mirrorA), Some(mirrorB)) =>
          val coproductMirrorA = CoproductMirror.fromMirror(mirrorA)
          val coproductMirrorB = CoproductMirror.fromMirror(mirrorB)
          Some(
            deriveCoproduct[A, B, Flags](
              source,
              materialized,
              coproductMirrorA,
              coproductMirrorB
            )
          )
        case _ =>
          None

    val containerLikeResult: Option[Expr[B]] = deriveAllContainerLike[A, B, Flags](source, materialized)

    productResult orElse coproductResult orElse containerLikeResult getOrElse
      report.errorAndAbort(
        s"Transformation from ${Type.show[A]} to ${Type.show[B]} not supported in chimney"
      )
  end deriveTransformer

end TransformerDeriveMacros

object TransformerDeriveMacros:
  def deriveTransformerMacro[
    A: Type,
    B: Type,
    Conf <: Tuple: Type,
    Flags <: Tuple: Type
  ](
    source: Expr[A],
    definition: Expr[TransformerDefinition[A, B, Conf, Flags]]
  )(using Quotes): Expr[B] =
    TransformerDeriveMacros(quotes).deriveTransformerMacro(source, definition)
end TransformerDeriveMacros
