package io.scalaland.chimney.internal.utils.modules

import io.scalaland.chimney.TransformerFSupport
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
        deriveIterableExtract[a, A, B, Flags](
          from,
          config,
          '{ a => a.asInstanceOf[Array[a]].iterator }
        )
      case '[IArray[a]] =>
        deriveIterableExtract[a, A, B, Flags](
          from,
          config,
          '{ a => a.asInstanceOf[Array[a]].iterator }
        )

      case '[Iterable[a]] =>
        deriveIterableExtract[a, A, B, Flags](
          from,
          config,
          '{ a => a.asInstanceOf[Iterable[a]].iterator }
        )

      case '[Iterable[(a1, a2)]] =>
        deriveMapLikeExtract[a1, a2, A, B, Flags](
          from,
          config,
          '{ a => a.asInstanceOf[IterableOnce[(a1, a2)]].iterator }
        )
      case _ =>
        deriveOption[A, B, Flags](from, config) orElse
          deriveEither[A, B, Flags](from, config)
  end deriveAllContainerLike

  def deriveAllContainerLikeF[
    F[_]: Type,
    A: Type,
    B: Type,
    Flags <: Tuple: Type
  ](
    from: Expr[A],
    config: TransformerFDefinitionMaterialized[Flags],
    support: Expr[TransformerFSupport[F]]
  ): Option[Expr[F[B]]] =
    Type.of[A] match
      case '[Array[a]] =>
        deriveIterableExtractF[F, a, A, B, Flags](
          from,
          config,
          support,
          '{ a => a.asInstanceOf[Array[a]].iterator }
        )
      case '[IArray[a]] =>
        deriveIterableExtractF[F, a, A, B, Flags](
          from,
          config,
          support,
          '{ a => a.asInstanceOf[Array[a]].iterator }
        )

      case '[Iterable[a]] =>
        deriveIterableExtractF[F, a, A, B, Flags](
          from,
          config,
          support,
          '{ a => a.asInstanceOf[Iterable[a]].iterator }
        )

      case '[Iterable[(a1, a2)]] =>
        deriveMapLikeExtractF[F, a1, a2, A, B, Flags](
          from,
          config,
          support,
          '{ a => a.asInstanceOf[Iterable[(a1, a2)]].iterator }
        )
      case _ =>
        deriveOptionF[F, A, B, Flags](from, config, support) orElse
          deriveEitherF[F, A, B, Flags](from, config, support)
  end deriveAllContainerLikeF

  private def deriveIterableExtract[
    A: Type,
    IA: Type,
    IB: Type,
    Flags <: Tuple: Type
  ](
    from: Expr[IA],
    config: TransformerDefinitionMaterialized[Flags],
    toIterator: Expr[IA => Iterator[A]]
  ): Option[Expr[IB]] =
    Type.of[IB] match
      case '[Array[b]] =>
        deriveIterable[A, b, IA, IB, Flags](from, config, toIterator)
      case '[IArray[b]] =>
        deriveIterable[A, b, IA, IB, Flags](from, config, toIterator)
      case '[IterableOnce[b]] =>
        deriveIterable[A, b, IA, IB, Flags](from, config, toIterator)
      case _ =>
        None
  end deriveIterableExtract

  private def deriveIterableExtractF[
    F[_]: Type,
    A: Type,
    IA: Type,
    IB: Type,
    Flags <: Tuple: Type
  ](
    from: Expr[IA],
    config: TransformerFDefinitionMaterialized[Flags],
    support: Expr[TransformerFSupport[F]],
    toIterator: Expr[IA => Iterator[A]]
  ): Option[Expr[F[IB]]] =
    Type.of[IB] match
      case '[Array[b]] =>
        deriveIterableF[F, A, b, IA, IB, Flags](
          from,
          config,
          support,
          toIterator
        )
      case '[IArray[b]] =>
        deriveIterableF[F, A, b, IA, IB, Flags](
          from,
          config,
          support,
          toIterator
        )
      case '[IterableOnce[b]] =>
        deriveIterableF[F, A, b, IA, IB, Flags](
          from,
          config,
          support,
          toIterator
        )
      case _ =>
        None
  end deriveIterableExtractF

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
  ): Option[Expr[IB]] =
    Type.of[IB] match
      case '[IterableOnce[(b1, b2)]] =>
        deriveMapLike[A1, A2, b1, b2, IA, IB, Flags](from, config, toIterator)
      case _ =>
        None
  end deriveMapLikeExtract

  private def deriveMapLikeExtractF[
    F[_]: Type,
    A1: Type,
    A2: Type,
    IA: Type,
    IB: Type,
    Flags <: Tuple: Type
  ](
    from: Expr[IA],
    config: TransformerFDefinitionMaterialized[Flags],
    support: Expr[TransformerFSupport[F]],
    toIterator: Expr[IA => Iterator[(A1, A2)]]
  ): Option[Expr[F[IB]]] =
    Type.of[IB] match
      case '[IterableOnce[(b1, b2)]] =>
        deriveMapLikeF[F, A1, A2, b1, b2, IA, IB, Flags](
          from,
          config,
          support,
          toIterator
        )
      case _ =>
        None
  end deriveMapLikeExtractF

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
  ): Option[Expr[IB]] =
    Expr.summon[Factory[B, IB]] match
      case Some(factory) =>
        Some(
          '{
            val builder = $factory.newBuilder
            for el <- $toIterator($from) do
              builder += ${ elemWiseTransform[A, B, Flags]('{ el }, config) }
            builder.result
          }
        )
      case None =>
        None
  end deriveIterable

  private def deriveIterableF[
    F[_]: Type,
    A: Type,
    B: Type,
    IA: Type,
    IB: Type,
    Flags <: Tuple: Type
  ](
    from: Expr[IA],
    config: TransformerFDefinitionMaterialized[Flags],
    support: Expr[TransformerFSupport[F]],
    toIterator: Expr[IA => Iterator[A]]
  ): Option[Expr[F[IB]]] =
    Expr.summon[Factory[B, IB]] match
      case Some(factory) =>
        Some(
          '{
            $support.traverse(
              $toIterator($from),
              a =>
                ${ elemWiseTransformF[F, A, B, Flags]('{ a }, config, support) }
            )(using $factory)
          }
        )
      case None =>
        None
  end deriveIterableF

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
  ): Option[Expr[IB]] =
    Expr.summon[Factory[(B1, B2), IB]] match
      case Some(factory) =>
        Some(
          '{
            val builder = $factory.newBuilder
            for (a1, a2) <- $toIterator($from) do
              builder += (${
                elemWiseTransform[A1, B1, Flags]('{ a1 }, config)
              } -> ${ elemWiseTransform[A2, B2, Flags]('{ a2 }, config) })
            builder.result
          }
        )
      case None =>
        None
  end deriveMapLike

  private def deriveMapLikeF[
    F[_]: Type,
    A1: Type,
    A2: Type,
    B1: Type,
    B2: Type,
    IA: Type,
    IB: Type,
    Flags <: Tuple: Type
  ](
    from: Expr[IA],
    config: TransformerFDefinitionMaterialized[Flags],
    support: Expr[TransformerFSupport[F]],
    toIterator: Expr[IA => Iterator[(A1, A2)]]
  ): Option[Expr[F[IB]]] =
    Expr.summon[Factory[(B1, B2), IB]] match
      case Some(factory) =>
        Some(
          '{
            $support.traverse(
              $toIterator($from),
              (a1, a2) =>
                $support.product(
                  ${
                    elemWiseTransformF[F, A1, B1, Flags](
                      '{ a1 },
                      config,
                      support
                    )
                  },
                  ${
                    elemWiseTransformF[F, A2, B2, Flags](
                      '{ a2 },
                      config,
                      support
                    )
                  }
                )
            )(using $factory)
          }
        )
      case None =>
        None
  end deriveMapLikeF

  private def deriveOptionF[F[_]: Type, A: Type, B: Type, Flags <: Tuple: Type](
    from: Expr[A],
    config: TransformerFDefinitionMaterialized[Flags],
    support: Expr[TransformerFSupport[F]]
  ): Option[Expr[F[B]]] =
    (from, Type.of[B]) match
      case ('{ $opt: Option[a] }, '[Option[b]]) =>
        Some(
          '{
            $opt match
              case Some(a) =>
                $support
                  .map(
                    ${
                      elemWiseTransformF[F, a, b, Flags](
                        '{ a },
                        config,
                        support
                      )
                    },
                    Some(_)
                  )
                  .asInstanceOf[F[B]]
              case None =>
                $support.pure(None).asInstanceOf[F[B]]
          }
        )
      case ('{ $opt: Option[a] }, _)
          if config.hasAFlag[TransformerFlag.UnsafeOption] =>
        Some(
          '{
            ${
              elemWiseTransformF[F, a, B, Flags]('{ $opt.get }, config, support)
            }
              .asInstanceOf[F[B]]
          }
        )
      case (_, '[Option[b]]) =>
        Some('{
          $support
            .map(
              ${ elemWiseTransformF[F, A, b, Flags](from, config, support) },
              Option(_)
            )
            .asInstanceOf[F[B]]
        })
      case _ =>
        None
  end deriveOptionF

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
      case (_, '[Option[b]]) =>
        Some('{
          Option(
            ${ elemWiseTransform[A, b, Flags](from, config) }
          )
            .asInstanceOf[B]
        })
      case _ =>
        None
  end deriveOption

  private def deriveEitherF[F[_]: Type, A: Type, B: Type, Flags <: Tuple: Type](
    from: Expr[A],
    config: TransformerFDefinitionMaterialized[Flags],
    support: Expr[TransformerFSupport[F]]
  ): Option[Expr[F[B]]] =
    (from, Type.of[B]) match
      case ('{ $either: Either[aa, ab] }, '[Either[ba, bb]]) =>
        Some(
          '{
            $either match
              case Left(a) =>
                $support
                  .map(
                    ${
                      elemWiseTransformF[F, aa, ba, Flags](
                        '{ a },
                        config,
                        support
                      )
                    },
                    Left(_)
                  )
                  .asInstanceOf[F[B]]
              case Right(b) =>
                $support
                  .map(
                    ${
                      elemWiseTransformF[F, ab, bb, Flags](
                        '{ b },
                        config,
                        support
                      )
                    },
                    Right(_)
                  )
                  .asInstanceOf[F[B]]
          }
        )
      case _ =>
        None
  end deriveEitherF

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
                Left(${ elemWiseTransform[aa, ba, Flags]('{ a }, config) })
                  .asInstanceOf[B]
              case Right(b) =>
                Right(${ elemWiseTransform[ab, bb, Flags]('{ b }, config) })
                  .asInstanceOf[B]
          }
        )
      case _ =>
        None
  end deriveEither

end CollectionDeriveModule
