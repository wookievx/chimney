package io.scalaland.chimney.internal.utils.modules

import io.scalaland.chimney.internal.TransformerFlag
import io.scalaland.chimney.internal.derived.TransformerDeriveMacros

import scala.collection.Factory
import scala.quoted.*
import scala.util.chaining.*

trait CollectionDeriveModule:
  self: TransformerDeriveMacros & CommonDeriveModule & FieldModule &
    MirrorModule & ConfigModule & Module =>
  import quotes.reflect.*

  def deriveAllContainerLike[
    A: Type,
    B: Type,
    Flags <: Tuple: Type
  ](
    from: Expr[A],
    config: TransformerDefinitionMaterialized[Flags]
  ): Option[Expr[B]] =
    Type.of[A] match
      case '[Array[a]] =>
        Some(
          deriveIterableExtract[a, A, B, Flags](
            from,
            config,
            '{ a => a.asInstanceOf[Array[a]].iterator }
          )
        )
      case '[IArray[a]] =>
        Some(
          deriveIterableExtract[a, A, B, Flags](
            from,
            config,
            '{ a => a.asInstanceOf[Array[a]].iterator }
          )
        )
      case '[IterableOnce[a]] =>
        Some(
          deriveIterableExtract[a, A, B, Flags](
            from,
            config,
            '{ a => a.asInstanceOf[IterableOnce[a]].iterator }
          )
        )
      case '[IterableOnce[(a1, a2)]] =>
        Some(
          deriveMapLikeExtract[a1, a2, A, B, Flags](
            from,
            config,
            '{ a => a.asInstanceOf[IterableOnce[(a1, a2)]].iterator }
          )
        )
      case _ =>
        deriveOption[A, B, Flags](from, config) orElse
        deriveEither[A, B, Flags](from, config)
  end deriveAllContainerLike

  private def deriveIterableExtract[
    A: Type,
    IA: Type,
    IB: Type,
    Flags <: Tuple: Type
  ](
    from: Expr[IA],
    config: TransformerDefinitionMaterialized[Flags],
    toIterator: Expr[IA => Iterator[A]]
  ): Expr[IB] =
    Type.of[IB] match
      case '[Array[b]] =>
        deriveIterable[A, b, IA, IB, Flags](from, config, toIterator)
      case '[IArray[b]] =>
        deriveIterable[A, b, IA, IB, Flags](from, config, toIterator)
      case '[IterableOnce[b]] =>
        deriveIterable[A, b, IA, IB, Flags](from, config, toIterator)
      case _ =>
        report.errorAndAbort(
          s"Convertion from ${Type.show[IA]} to ${Type.show[IB]} not supported"
        )
  end deriveIterableExtract

  private def deriveMapLikeExtract[
    A1: Type,
    A2: Type,
    IA: Type,
    IB: Type,
    Flags <: Tuple: Type
  ](
    from: Expr[IA],
    config: TransformerDefinitionMaterialized[Flags],
    toIterator: Expr[IA => Iterator[(A1, A2)]]
  ): Expr[IB] =
    Type.of[IB] match
      case '[IterableOnce[(b1, b2)]] =>
        deriveMapLike[A1, A2, b1, b2, IA, IB, Flags](from, config, toIterator)
      case _ =>
        report.errorAndAbort(
          s"Convertion from ${Type.show[IA]} to ${Type.show[IB]} not supported"
        )
  end deriveMapLikeExtract

  private def deriveIterable[
    A: Type,
    B: Type,
    IA: Type,
    IB: Type,
    Flags <: Tuple: Type
  ](
    from: Expr[IA],
    config: TransformerDefinitionMaterialized[Flags],
    toIterator: Expr[IA => Iterator[A]]
  ): Expr[IB] =
    Expr.summon[Factory[B, IB]] match
      case Some(factory) =>
        '{
          val builder = $factory.newBuilder
          for el <- $toIterator($from) do
            builder += ${ elemWiseTransform[A, B, Flags]('{ el }, config) }
          builder.result
        }
      case None =>
        report.errorAndAbort(
          s"Missing factory for collection type: ${Type.show[IB]}"
        )
  end deriveIterable

  private def deriveMapLike[
    A1: Type,
    A2: Type,
    B1: Type,
    B2: Type,
    IA: Type,
    IB: Type,
    Flags <: Tuple: Type
  ](
    from: Expr[IA],
    config: TransformerDefinitionMaterialized[Flags],
    toIterator: Expr[IA => Iterator[(A1, A2)]]
  ): Expr[IB] =
    Expr.summon[Factory[(B1, B2), IB]] match
      case Some(factory) =>
        '{
          val builder = $factory.newBuilder
          for (a1, a2) <- $toIterator($from) do
            builder += (${
              elemWiseTransform[A1, B1, Flags]('{ a1 }, config)
            } -> ${ elemWiseTransform[A2, B2, Flags]('{ a2 }, config) })
          builder.result
        }
      case None =>
        report.errorAndAbort(
          s"Missing factory for collection type: ${Type.show[IB]}"
        )
  end deriveMapLike

  private def deriveOption[A: Type, B: Type, Flags <: Tuple: Type](
    from: Expr[A],
    config: TransformerDefinitionMaterialized[Flags]
  ): Option[Expr[B]] =
    (from, Type.of[B]) match
      case ('{ $opt: Option[a] }, '[Option[b]]) =>
        Some('{
          $opt
            .map(a => ${ elemWiseTransform[a, b, Flags]('{ a }, config) })
            .asInstanceOf[B]
        })
      case ('{ $opt: Option[a] }, _)
          if config.hasAFlag[TransformerFlag.UnsafeOption] =>
        Some('{
          ${ elemWiseTransform[a, B, Flags]('{ $opt.get }, config) }
            .asInstanceOf[B]
        })
      case _ =>
        None
  end deriveOption

  private def deriveEither[A: Type, B: Type, Flags <: Tuple: Type](
    from: Expr[A],
    config: TransformerDefinitionMaterialized[Flags]
  ): Option[Expr[B]] =
    (from, Type.of[B]) match
      case ('{ $either: Either[aa, ab] }, '[Either[ba, bb]]) =>
        Some(
          '{
            $either match
              case Left(a) =>
                Left(${ elemWiseTransform[aa, ba, Flags]('{ a }, config) }).asInstanceOf[B]
              case Right(b) =>
                Right(${ elemWiseTransform[ab, bb, Flags]('{ b }, config) }).asInstanceOf[B]
          }
        )
      case _ =>
        None
  end deriveEither

end CollectionDeriveModule
