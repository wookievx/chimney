package io.scalaland.chimney
package internal.derived

import io.scalaland.chimney.*
import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.internal.utils.*
import io.scalaland.chimney.internal.*

import scala.compiletime.ops.int.*
import scala.compiletime.*
import scala.deriving.*
import scala.quoted.*

object BeanGettersDerive:
  import DeriveUtils.*
  import TransformerCfg.*

  inline def deriveFromJavaBean[From, To](
    inline config: TypeDeriveConfig[_, _, _],
    tm: Mirror.ProductOf[To]
  ): Transformer[From, To] =
    new Transformer[From, To]:
      def transform(from: From): To =
        val output =
          new Array[Any](constValue[Tuple.Size[tm.MirroredElemTypes]])
        handleTargetFields[
          From,
          To,
          tm.MirroredElemTypes,
          tm.MirroredElemLabels,
          0
        ](config)(index => value => output(index) = value, from)
        tm.fromProduct(ArrayProduct(output))
      end transform

  inline def deriveFromJavaBeanF[F[_], From, To](
    inline config: TypeDeriveConfig[_, _, _],
    tm: Mirror.ProductOf[To]
  )(using sup: TransformerFSupport[F]): TransformerF[F, From, To] =
    new TransformerF[F, From, To]:
      def transform(from: From): F[To] =
        val output =
          new Array[Any](constValue[Tuple.Size[tm.MirroredElemTypes]])
        val effect = handleTargetFieldsF[
          F,
          From,
          To,
          tm.MirroredElemTypes,
          tm.MirroredElemLabels,
          0
        ](config)(
          index => value => sup.map(value, v => output(index) = v),
          from
        )
        sup.map(effect, _ => tm.fromProduct(ArrayProduct(output)))
      end transform

  inline def handleTargetFields[
    From,
    To,
    ToValues <: Tuple,
    ToLabels <: Tuple,
    AtPos <: Int
  ](
    inline config: TypeDeriveConfig[_, _, _]
  )(
    ouputFun: Int => Any => Unit,
    input: From
  ): Unit =
    inline (erasedValue[ToValues], erasedValue[ToLabels]) match
      case _: ((fieldType *: toValues), (fieldLabel *: toLabels)) =>
        handleTargetField[AtPos, fieldLabel, fieldType, From, To](config)(
          ouputFun,
          input
        )
        handleTargetFields[From, To, toValues, toLabels, AtPos + 1](config)(
          ouputFun,
          input
        )
      case _ =>
  end handleTargetFields

  inline def handleTargetFieldsF[
    F[_],
    From,
    To,
    ToValues <: Tuple,
    ToLabels <: Tuple,
    AtPos <: Int
  ](
    inline config: TypeDeriveConfig[_, _, _]
  )(
    ouputFun: Int => F[Any] => F[Unit],
    input: From
  )(using sup: TransformerFSupport[F]): F[Unit] =
    inline (erasedValue[ToValues], erasedValue[ToLabels]) match
      case _: ((fieldType *: toValues), (fieldLabel *: toLabels)) =>
        handleTargetFieldF[F, AtPos, fieldLabel, fieldType, From, To](config)(
          ouputFun,
          input
        )
        handleTargetFieldsF[F, From, To, toValues, toLabels, AtPos + 1](config)(
          ouputFun,
          input
        )
      case _ => sup.pure(())
  end handleTargetFieldsF

  inline def handleTargetField[At <: Int, Name, T, From, To](
    inline config: TypeDeriveConfig[_, _, _]
  )(
    ouputFun: Int => Any => Unit,
    input: From
  ): Unit =
    inline config match
      case config: TypeDeriveConfig[config, flags, _] =>
        inline erasedValue[ConfigOf[config, Name]] match
          case _: FieldConst[_] =>
            ouputFun(constValue[At])(
              config.overrides(constValue[Name].asInstanceOf)
            )
          case _: FieldComputed[_] =>
            val f = config
              .overrides(constValue[Name].asInstanceOf)
              .asInstanceOf[From => Any]
            ouputFun(constValue[At])(f(input))
          case _: FieldRelabelled[fieldFrom, Name] =>
            extractFromSource[fieldFrom, T, From, To, flags](config)(
              ouputFun(constValue[At]),
              input
            )
          case _ =>
            extractFromSource[Name, T, From, To, flags](config)(
              ouputFun(constValue[At]),
              input
            )
  end handleTargetField

  inline def extractFromSource[LabelTo, TypeTo, From, To, Flags <: Tuple](
    inline config: TypeDeriveConfig[_, Flags, _]
  )(outputFun: Any => Unit, typedInput: From): Unit =
    outputFun(
      extractFromInput[From, To, TypeTo, LabelTo](
        config,
        typedInput
      )
    )
  end extractFromSource

  private inline def extractFromInput[From, To, FTo, LabelTo](
    inline config: TypeDeriveConfig[_, _, _],
    typedInput: From
  ): FTo =
    inline config match
      case c: TypeDeriveConfig[_, flags, path] =>
        inline ClassAccessMacros.selectBeanGetter(
          typedInput,
          constValue[LabelTo]
        ) match
          case res: Some[FTo] =>
            res.get
          case res: Some[fTo] =>
            inline MacroUtils.attemptSummonInstance[Transformer[fTo, FTo]] match
              case Some(transformer: Transformer[fTo, FTo]) =>
                transformer.transform(res.get)
              case _ =>
                val transformer = TransformerDerive
                  .deriveConfigured[
                    fTo,
                    FTo,
                    path Concat "." Concat LabelTo
                  ](
                    configOfAtPath[
                      FTo,
                      flags,
                      path Concat "." Concat LabelTo
                    ](
                      defaultDefinitionWithFlags
                    ),
                    constValue[HasAFlag[flags, TransformerFlag.BeanGetters]],
                    constValue[HasAFlag[flags, TransformerFlag.BeanSetters]]
                  )
                transformer.transform(res.get)
          case None =>
            extractDefault[flags, FTo, LabelTo](c) match
              case Some(v: FTo) => v
              case _ =>
                MacroUtils.reportErrorAtPath(
                  constValue[path],
                  MacroUtils.printfCompileTime[
                    "Automatic derivation from %s to %s failed, unable to extract value of field: %s",
                    (From, To, LabelTo)
                  ]
                )

  inline def handleTargetFieldF[F[_], At <: Int, Name, T, From, To](
    inline config: TypeDeriveConfig[_, _, _]
  )(
    ouputFun: Int => F[Any] => F[Unit],
    input: From
  )(using sup: TransformerFSupport[F]): F[Unit] =
    inline config match
      case config: TypeDeriveConfig[config, flags, _] =>
        inline erasedValue[ConfigOf[config, Name]] match
          case _: FieldConst[_] =>
            ouputFun(constValue[At])(
              sup.pure(config.overrides(constValue[Name].asInstanceOf))
            )
          case _: FieldComputed[_] =>
            val f = config
              .overrides(constValue[Name].asInstanceOf)
              .asInstanceOf[From => Any]
            ouputFun(constValue[At])(sup.pure(f(input)))
          case _: FieldComputedF[_] =>
            val f = config
              .overrides(constValue[Name].asInstanceOf)
              .asInstanceOf[From => F[Any]]
            ouputFun(constValue[At])(f(input))
          case _: FieldRelabelled[fieldFrom, Name] =>
            extractFromSourceF[F, fieldFrom, T, From, To, flags](config)(
              ouputFun(constValue[At]),
              input
            )
          case _ =>
            extractFromSourceF[F, Name, T, From, To, flags](config)(
              ouputFun(constValue[At]),
              input
            )
  end handleTargetFieldF

  inline def extractFromSourceF[F[
    _
  ], LabelTo, TypeTo, From, To, Flags <: Tuple](
    inline config: TypeDeriveConfig[_, Flags, _]
  )(outputFun: F[Any] => F[Unit], typedInput: From)(using
    sup: TransformerFSupport[F]
  ): F[Unit] =
    outputFun {
     extractFromInput[From, To, TypeTo, LabelTo](
          config,
          typedInput
        ).asInstanceOf[F[Any]]
    }
  end extractFromSourceF

  private inline def extractFromInputF[F[_], From, To, FTo, LabelTo](
    inline config: TypeDeriveConfig[_, _, _],
    typedInput: From
  )(using
    sup: TransformerFSupport[F]
  ): F[FTo] =
    inline config match
      case c: TypeDeriveConfig[_, flags, path] =>
        inline ClassAccessMacros.selectBean(
          typedInput,
          constValue[LabelTo]
        ) match
          case res: Some[F[FTo]] =>
            res.get
          case res: Some[FTo] =>
            sup.pure(res.get)
          case res: Some[fTo] =>
            inline MacroUtils
              .attemptSummonInstance[TransformerF[F, fTo, FTo]] match
              case Some(transformer: TransformerF[F, fTo, FTo]) =>
                transformer.transform(res.get)
              case _ =>
                val transformer = TransformerDerive
                  .deriveConfiguredF[
                    F,
                    fTo,
                    FTo,
                    path Concat "." Concat LabelTo
                  ](
                    configOfAtPath[
                      FTo,
                      flags,
                      path Concat "." Concat LabelTo
                    ](
                      defaultDefinitionWithFlags
                    ),
                    constValue[HasAFlag[flags, TransformerFlag.BeanGetters]]
                  )
                transformer.transform(res.get)
          case None =>
            extractDefault[flags, FTo, LabelTo](c) match
              case Some(v: FTo) => sup.pure(v)
              case _ =>
                MacroUtils.reportErrorAtPath(
                  constValue[path],
                  MacroUtils.printfCompileTime[
                    "Automatic derivation from %s to %s failed, unable to extract value of field: %s",
                    (From, To, LabelTo)
                  ]
                )

  private transparent inline def extractDefault[Flags <: Tuple, To, Field](
    inline config: TypeDeriveConfig[_, Flags, _]
  ) =
    inline if constValue[HasAFlag[Flags, TransformerFlag.DefaultValues]] then
      inline if MacroUtils.defaultValueExistsIn[To](constValue[Field]) then
        Some(config.defaults(constValue[Field].asInstanceOf))
      else None
    else None
  end extractDefault

end BeanGettersDerive
