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

object BeanSettersDerive:
  import DeriveUtils.*
  import TransformerCfg.*

  inline def deriveToJavaBean[From, To, Path <: String](
    inline config: TypeDeriveConfig[_, _, Path],
    fm: Mirror.ProductOf[From]
  ): Transformer[From, To] = new:
    def transform(from: From): To =
      inline ClassAccessMacros.getEmptyInstance[To] match
        case Some(instance) =>
          val input = Tuple.fromProduct(from.asInstanceOf[Product]).toIArray
          handleTargetFields[
            From,
            To,
            fm.MirroredElemLabels,
            fm.MirroredElemTypes
          ](config)(instance, input)
          instance
        case None =>
          MacroUtils.reportErrorAtPath(
            constValue[Path],
            MacroUtils.printfCompileTime[
              "Automatic derivation from %s to %s failed, unable to construct value of %s with empty constructor",
              (From, To, To)
            ]
          )
    end transform

  inline def handleTargetFields[
    From,
    To,
    FromLabels <: Tuple,
    FromValues <: Tuple
  ](
    inline config: TypeDeriveConfig[_, _, _],
    inline pos: Int = 0
  )(
    output: To,
    input: IArray[Any]
  ): Unit =
    inline (erasedValue[FromLabels], erasedValue[FromValues]) match
      case (_: (name *: fromLabels), _: (tfrom *: fromValues)) =>
        handleTargetField[name, tfrom, To](
          config
        )(output, input(pos).asInstanceOf[tfrom])
        handleTargetFields[From, To, fromLabels, fromValues](
          config,
          pos + 1
        )(
          output,
          input
        )
      case _ =>
  end handleTargetFields

  inline def handleTargetField[Name, TFrom, To](
    inline config: TypeDeriveConfig[_, _, _]
  )(
    output: To,
    input: TFrom
  ): Unit =
    inline ClassAccessMacros.selectBeanGetter(output, constValue[Name]) match
      case _: Some[TFrom] =>
        ClassAccessMacros.selectBeanSetter(output, constValue[Name], input)
      case _: Some[fTo] =>
        inline MacroUtils.attemptSummonInstance[Transformer[TFrom, fTo]] match
          case Some(transformer: Transformer[TFrom, fTo]) =>
            ClassAccessMacros.selectBeanSetter(
              output,
              constValue[Name],
              transformer.transform(input)
            )
          case None =>
            inline config match
              case _: TypeDeriveConfig[_, flags, path] =>
                val transformer = TransformerDerive
                  .deriveConfigured[
                    TFrom,
                    fTo,
                    path Concat "." Concat Name
                  ](
                    configOfAtPath[
                      fTo,
                      flags,
                      path Concat "." Concat Name
                    ](
                      defaultDefinitionWithFlags
                    ),
                    constValue[HasAFlag[flags, TransformerFlag.BeanGetters]],
                    constValue[HasAFlag[flags, TransformerFlag.BeanSetters]]
                  )
                ClassAccessMacros.selectBeanSetter(
                  output,
                  constValue[Name],
                  transformer.transform(input)
                )
  end handleTargetField

end BeanSettersDerive
