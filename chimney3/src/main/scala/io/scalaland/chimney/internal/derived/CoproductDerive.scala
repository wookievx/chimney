package io.scalaland.chimney.internal.derived

import io.scalaland.chimney._
import io.scalaland.chimney.dsl._
import io.scalaland.chimney.internal.utils.MacroUtils
import io.scalaland.chimney.internal._
import scala.compiletime.ops.int._
import scala.compiletime._
import scala.deriving._

object CoproductDerive:
  import DeriveUtils._

  inline def derived[From, To, Config <: Tuple, Path <: String](
    inline config: TypeDeriveConfig[Config, _, Path]
  )(using fm: Mirror.SumOf[From], tm: Mirror.SumOf[To]): Transformer[From, To] =
    DeriveUtils.transformerWith[From, To] { from =>
      findACase[
        From,
        To,
        fm.MirroredElemTypes,
        tm.MirroredElemTypes,
        fm.MirroredElemLabels,
        Config,
        0
      ](config)(from)
    }
  end derived

  inline def derivedF[F[_], From, To, Config <: Tuple, Path <: String](
    inline config: TypeDeriveConfig[Config, _, Path]
  )(using
    fm: Mirror.SumOf[From],
    tm: Mirror.SumOf[To],
    sup: TransformerFSupport[F]
  ): TransformerF[F, From, To] =
    DeriveUtils.transformerWithF[F, From, To](from =>
      findACaseF[
        F,
        From,
        To,
        fm.MirroredElemTypes,
        tm.MirroredElemTypes,
        fm.MirroredElemLabels,
        Config,
        0
      ](config)(from)
    )
  end derivedF

  inline def findACase[
    From,
    To,
    FromLeft <: Tuple,
    ToLeft <: Tuple,
    Labels <: Tuple,
    Config <: Tuple,
    Position <: Int
  ](
    inline config: TypeDeriveConfig[Config, _, _]
  )(from: From)(using fm: Mirror.SumOf[From]): To =
    inline (
      erasedValue[FromLeft],
      erasedValue[ToLeft],
      erasedValue[Labels]
    ) match
      case _: (from *: fromLeft, to *: toLeft, fromName *: labels) =>
        if fm.ordinal(from) == constValue[Position] then
          inline config match
            case _: TypeDeriveConfig[_, flags, path] =>
              inline extractInstanceAt[Config, from](config) match
                case f: Some[`from` => tt] =>
                  f.get.asInstanceOf[From => To](from)
                case _ =>
                  TransformerDerive
                    .deriveConfigured[
                      from,
                      to,
                      path Concat "." Concat fromName
                    ](
                      configOfAtPath[
                        to,
                        flags,
                        path Concat "." Concat fromName
                      ](defaultDefinitionWithFlags),
                      constValue[HasAFlag[flags, TransformerFlag.BeanGetters]]
                    )
                    .asInstanceOf[Transformer[From, To]]
                    .transform(from)
        else
          findACase[From, To, fromLeft, toLeft, labels, Config, Position + 1](
            config
          )(from)
      case _: (EmptyTuple, _, _) =>
        throw new Exception(
          "Should not be here, bug in implementation, report it"
        )
      case _: (from *: _, EmptyTuple, _) =>
        inline extractInstanceAt[Config, from](
          config.asInstanceOf[TypeDeriveConfig[Config, _, _]]
        ) match
          case f: Some[`from` => tt] => f.get.asInstanceOf[From => To](from)
          case _ =>
            inline config match
              case _: TypeDeriveConfig[_, _, path] =>
                MacroUtils.reportErrorAtPathWithType[
                  path,
                  "Structure of transformed coproducts do not match",
                  (From, To)
                ]("Transforming coproducts")
  end findACase

  inline def findACaseF[F[
    _
  ], From, To, FromLeft <: Tuple, ToLeft <: Tuple, Labels <: Tuple, Config <: Tuple, Position <: Int](
    inline config: TypeDeriveConfig[Config, _, _]
  )(
    from: From
  )(using
    fm: Mirror.SumOf[From],
    sup: TransformerFSupport[F]
  ): F[To] =
    inline (
      erasedValue[FromLeft],
      erasedValue[ToLeft],
      erasedValue[Labels]
    ) match
      case _: (from *: fromLeft, to *: toLeft, fromName *: labels) =>
        if fm.ordinal(from) == constValue[Position] then
          inline config match
            case _: TypeDeriveConfig[_, flags, path] =>
              inline extractInstanceAtF[F, Config, from](config) match
                case f: Some[`from` => F[tt]] =>
                  f.get.asInstanceOf[From => F[To]](from)
                case f: Some[`from` => tt] =>
                  sup.pure(f.get.asInstanceOf[From => To](from))
                case d =>
                  TransformerDerive
                    .deriveConfiguredF[
                      F,
                      from,
                      to,
                      path Concat "." Concat fromName
                    ](
                      configOfAtPath[
                        to,
                        flags,
                        path Concat "." Concat fromName
                      ](defaultDefinitionWithFlags)
                    )
                    .asInstanceOf[TransformerF[F, From, To]]
                    .transform(from)
        else
          findACaseF[
            F,
            From,
            To,
            fromLeft,
            toLeft,
            labels,
            Config,
            Position + 1
          ](config)(from)
      case _: (EmptyTuple, _, _) =>
        throw new Exception(
          "Should not be here, bug in implementation, report it"
        )
      case _: (from *: _, EmptyTuple, _) =>
        inline extractInstanceAtF[F, Config, from](config) match
          case f: Some[`from` => tt] =>
            sup.pure(f.get.asInstanceOf[From => To](from))
          case f: Some[`from` => F[tt]] =>
            f.get.asInstanceOf[From => F[To]](from)
          case _ =>
            inline config match
              case _: TypeDeriveConfig[_, _, path] =>
                MacroUtils.reportErrorAtPathWithType[
                  path,
                  "Structure of transformed coproducts do not match",
                  (From, To)
                ]("Transforming coproducts")
  end findACaseF

  private transparent inline def extractInstanceAtF[F[_], Config <: Tuple, FF](
    inline config: TypeDeriveConfig[Config, _, _]
  ) =
    inline erasedValue[InstanceConfigOf[Config, FF]] match
      case _: TransformerCfg.CoproductInstance[FF, tt] =>
        Some(
          config
            .instances((MacroUtils.showTypeVal[FF], MacroUtils.showTypeVal[tt]))
            .asInstanceOf[FF => tt]
        )
      case _: TransformerCfg.CoproductInstanceF[FF, tt] =>
        Some(
          config
            .instances((MacroUtils.showTypeVal[FF], MacroUtils.showTypeVal[tt]))
            .asInstanceOf[FF => F[tt]]
        )
      case _ =>
        None
  end extractInstanceAtF

  private transparent inline def extractInstanceAt[Config <: Tuple, FF](
    inline config: TypeDeriveConfig[Config, _, _]
  ) =
    inline erasedValue[InstanceConfigOf[Config, FF]] match
      case _: TransformerCfg.CoproductInstance[FF, tt] =>
        Some(
          config
            .instances((MacroUtils.showTypeVal[FF], MacroUtils.showTypeVal[tt]))
            .asInstanceOf[FF => tt]
        )
      case _ =>
        None
  end extractInstanceAt

end CoproductDerive
