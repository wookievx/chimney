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
