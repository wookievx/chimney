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
    overrides: Expr[Array[Any]],
    instances: Expr[Array[Any]]
  ): Expr[B] =
    val materialized =
      TransformerDefinitionMaterialized.materialize[B, Conf, Flags](
        overrides,
        instances
      )
    deriveTransformer[A, B, Flags](source, materialized)
  end deriveTransformerMacro

  def deriveTransformerFMacro[
    F[_]: Type,
    A: Type,
    B: Type,
    Conf <: Tuple: Type,
    Flags <: Tuple: Type
  ](
    source: Expr[A],
    overrides: Expr[Array[Any]],
    instances: Expr[Array[Any]],
    support: Expr[TransformerFSupport[F]]
  ): Expr[F[B]] =
    val materialized =
      TransformerFDefinitionMaterialized.materialize[B, Conf, Flags](
        overrides,
        instances
      )
    deriveTransformerF[F, A, B, Flags](source, materialized, support)
  end deriveTransformerFMacro

  private[internal] def deriveTransformer[
    A: Type,
    B: Type,
    Flags <: Tuple: Type
  ](
    source: Expr[A],
    materialized: TransformerDefinitionMaterialized[Flags]
  ): Expr[B] =
    def productResult: Option[Expr[B]] =
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
          else None
          end if
        case _ =>
          None

    def coproductResult: Option[Expr[B]] =
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

    def containerLikeResult: Option[Expr[B]] =
      deriveAllContainerLike[A, B, Flags](source, materialized)

    val derivationResult = containerLikeResult orElse productResult orElse coproductResult

    derivationResult getOrElse
      report.errorAndAbort(
        s"Transformation from ${Type.show[A]} to ${Type.show[B]} not supported in chimney"
      )
  end deriveTransformer

  private[internal] def deriveTransformerF[
    F[_]: Type,
    A: Type,
    B: Type,
    Flags <: Tuple: Type
  ](
    source: Expr[A],
    materialized: TransformerFDefinitionMaterialized[Flags],
    support: Expr[TransformerFSupport[F]]
  ): Expr[F[B]] =
    def productResult: Option[Expr[F[B]]] =
      (Expr.summon[Mirror.ProductOf[A]], Expr.summon[Mirror.ProductOf[B]]) match
        case (Some(mirrorA), Some(mirrorB)) =>
          val productMirrorA = ProductMirror.fromMirror(mirrorA)
          val productMirrorB = ProductMirror.fromMirror(mirrorB)
          Some(
            deriveProductF[F, A, B, Flags](
              source,
              materialized,
              support,
              productMirrorA,
              productMirrorB
            )
          )
        case (None, Some(mirrorB)) =>
          if materialized.hasAFlag[TransformerFlag.BeanGetters] then
            val productMirrorB = ProductMirror.fromMirror(mirrorB)
            Some(
              deriveProductFromJavaBeanF[F, A, B, Flags](
                source,
                materialized,
                support,
                productMirrorB
              )
            )
          else None
          end if
        case _ =>
          None

    def coproductResult: Option[Expr[F[B]]] =
      (Expr.summon[Mirror.SumOf[A]], Expr.summon[Mirror.SumOf[B]]) match
        case (Some(mirrorA), Some(mirrorB)) =>
          val coproductMirrorA = CoproductMirror.fromMirror(mirrorA)
          val coproductMirrorB = CoproductMirror.fromMirror(mirrorB)
          Some(
            deriveCoproductF[F, A, B, Flags](
              source,
              materialized,
              support,
              coproductMirrorA,
              coproductMirrorB
            )
          )
        case _ =>
          None

    def containerLikeResult: Option[Expr[F[B]]] =
      deriveAllContainerLikeF[F, A, B, Flags](source, materialized, support)

    val derivationResult = containerLikeResult orElse productResult orElse coproductResult

    derivationResult getOrElse
      report.errorAndAbort(
        s"Transformation from ${Type.show[A]} to ${Type.show[B]} not supported in chimney"
      )
  end deriveTransformerF

end TransformerDeriveMacros

object TransformerDeriveMacros:
  def deriveTransformerMacro[
    A: Type,
    B: Type,
    Conf <: Tuple: Type,
    Flags <: Tuple: Type
  ](
    definition: Expr[TransformerDefinition[A, B, Conf, Flags]]
  )(using Quotes): Expr[Transformer[A, B]] =
    val macros = TransformerDeriveMacros(quotes)
    val optimizedConfig = macros.OptimizedConfig.optimizedConfig[Conf](
      '{ $definition.overrides },
      '{ $definition.instances }
    )
    '{
      new Transformer[A, B]:
        private val overridesValues = ${ optimizedConfig.overrides }
        private val instancesValues = ${ optimizedConfig.instances }
        override def transform(from: A): B = ${
          macros.deriveTransformerMacro[A, B, Conf, Flags](
            '{ from },
            '{ overridesValues },
            '{ instancesValues }
          )
        }
    }

  def deriveTransformerFMacro[
    F[_]: Type,
    A: Type,
    B: Type,
    Conf <: Tuple: Type,
    Flags <: Tuple: Type
  ](
    definition: Expr[TransformerFDefinition[F, A, B, Conf, Flags]],
    support: Expr[TransformerFSupport[F]]
  )(using Quotes): Expr[TransformerF[F, A, B]] =
    val macros = TransformerDeriveMacros(quotes)
    val optimizedConfig = macros.OptimizedConfig.optimizedConfig[Conf](
      '{ $definition.overrides },
      '{ $definition.instances }
    )
    '{
      new TransformerF[F, A, B]:
        private val overridesValues = ${ optimizedConfig.overrides }
        private val instancesValues = ${ optimizedConfig.instances }
        override def transform(from: A): F[B] = ${
          macros.deriveTransformerFMacro[F, A, B, Conf, Flags](
            '{ from },
            '{ overridesValues },
            '{ instancesValues },
            support
          )
        }
    }
  end deriveTransformerFMacro

end TransformerDeriveMacros
