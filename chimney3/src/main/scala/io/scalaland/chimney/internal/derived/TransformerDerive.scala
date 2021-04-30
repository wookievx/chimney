package io.scalaland.chimney.internal.derived

import io.scalaland.chimney._
import io.scalaland.chimney.dsl._
import io.scalaland.chimney.internal.utils._
import io.scalaland.chimney.internal._

import scala.compiletime.ops.int._
import scala.compiletime._
import scala.deriving._
import scala.quoted._

object TransformerDerive:
  import DeriveUtils._
  import TransformerCfg._

  inline def derived[From, To, Config <: Tuple, Flags <: Tuple](config: TransformerDefinition[From, To, Config, Flags]): Transformer[From, To] =
    deriveConfigured[From, To, ""](configOf(config))

  inline def derived[F[_], From, To, Config <: Tuple, Flags <: Tuple](config: TransformerFDefinition[F, From, To, Config, Flags])(using sup: TransformerFSupport[F]): TransformerF[F, From, To] =
    deriveConfiguredF[F, From, To, ""](configOf(config))
 
  inline def deriveConfigured[From, To, P <: String](inline config: TypeDeriveConfig[_, _, P]): Transformer[From, To] =
    inline erasedValue[To] match
      case _: From => transformerWith(x => x.asInstanceOf[To])
      case _ =>
        inline config match
          case _: TypeDeriveConfig[_, flags, _] =>
            inline SpecialDerive.deriveSpecialCases[From, To, flags, P] match       
              case Some(transformer) =>
                transformer
              case None =>
                summonFrom {
                  case fm: Mirror.ProductOf[From] =>
                    summonFrom {
                      case tm: Mirror.ProductOf[To] =>
                        transformerWith[From, To] { from =>
                          val input = Tuple.fromProduct(from.asInstanceOf[Product]).toIArray
                          val output = new Array[Any](constValue[Tuple.Size[tm.MirroredElemTypes]])
                          DeriveProduct.handleTargetImpl[From, To, tm.MirroredElemTypes, tm.MirroredElemLabels, 0](config, fm)(output.asInstanceOf, input.asInstanceOf, from)
                          tm.fromProduct(ArrayProduct(output))
                        }
                      case _ =>
                        MacroUtils.reportErrorAtPath[P](
                          constValue[P], 
                          MacroUtils.printfCompileTime["Automatic derivation not supported (yet) for supplied types (from %s to %s)", (From, To)]
                        )
                    }
                  case fm: Mirror.SumOf[From] =>
                    summonFrom {
                      case tm: Mirror.SumOf[To] =>
                        inline config match
                          case c: TypeDeriveConfig[config, _, P] =>
                            CoproductDerive.derived[From, To, config, P](c)(using fm, tm)
                      case _ =>
                        MacroUtils.reportErrorAtPath[P](
                          constValue[P], 
                          MacroUtils.printfCompileTime["Automatic derivation not supported (yet) for supplied types (from %s to %s)", (From, To)]
                        )
                    }
                  case _ =>
                    MacroUtils.reportErrorAtPath[P](
                      constValue[P],
                      MacroUtils.printfCompileTime["Automatic derivation not supported (yet) for supplied types (from %s to %s)", (From, To)]
                    )
                }
  end deriveConfigured

  inline def deriveConfiguredF[F[_], From, To, P <: String](inline
   config: TypeDeriveConfig[_, _, P]
  )(using sup: TransformerFSupport[F]): TransformerF[F, From, To] = 
    inline erasedValue[To] match
      case _: From =>
        transformerWithF[F, From, To](x => sup.pure(x.asInstanceOf[To]))
      case _ =>
        inline config match
          case _: TypeDeriveConfig[_, flags, _] =>
            inline SpecialDerive.deriveSpecialCasesF[F, From, To, flags, P] match       
              case Some(transformer: Transformer[From, To]) =>
                liftTransformer(transformer)
              case Some(transformer: TransformerF[F, From, To]) =>
                transformer
              case _ =>
                summonFrom {
                  case fm: Mirror.ProductOf[From] =>
                    summonFrom {
                      case tm: Mirror.ProductOf[To] =>
                        transformerWithF[F, From, To] { from =>
                          val input = Tuple.fromProduct(from.asInstanceOf[Product]).toIArray
                          val output = sup.pure(new Array[Any](constValue[Tuple.Size[tm.MirroredElemTypes]]))
                          val processedOutput = DeriveProduct.handleTargetWithFImpl[F, From, To, tm.MirroredElemTypes, tm.MirroredElemLabels, 0](config, fm)(output.asInstanceOf, input.asInstanceOf, from)
                          sup.map(processedOutput, output => tm.fromProduct(ArrayProduct(output)))
                        }
                      case _ =>
                        MacroUtils.reportErrorAtPath[P](
                          constValue[P],
                          MacroUtils.printfCompileTime["Automatic derivation not supported (yet) for supplied types (from %s to %s)", (From, To)]
                        )
                    }
                  case fm: Mirror.SumOf[From] =>
                    summonFrom {
                      case tm: Mirror.SumOf[To] =>
                        inline config match
                          case c: TypeDeriveConfig[config, _, P] =>
                            CoproductDerive.derivedF[F, From, To, config, P](c)(using fm, tm)
                      case _ =>
                        MacroUtils.reportErrorAtPath[P](
                          constValue[P],
                          MacroUtils.printfCompileTime["Automatic derivation not supported (yet) for supplied types (from %s to %s)", (From, To)]
                        )
                    }
                  case _ =>
                    MacroUtils.reportErrorAtPath[P](
                      constValue[P],
                      MacroUtils.printfCompileTime["Automatic derivation not supported (yet) for supplied types (from %s to %s)", (From, To)]
                    )
                }
end TransformerDerive

object DeriveProduct:

  import DeriveUtils._
  import TransformerCfg._

  inline def handleTargetImpl[From, To, ToValues <: Tuple, TLabels <: Tuple, AtPos <: Int](
    inline config: TypeDeriveConfig[_, _, _],
    fm: Mirror.ProductOf[From]
  )(
    outputArray: Array[Any],
    inputArray: IArray[Any],
    input: From
  ): Unit = 
    inline (erasedValue[ToValues], erasedValue[TLabels]) match
      case _: ((fieldType *: tailValues), (field *: tailLabels)) =>
        handleTargetField[AtPos, field, fieldType, From, To](config, fm)(outputArray, inputArray, input)
        handleTargetImpl[From, To, tailValues, tailLabels, AtPos + 1](config, fm)(outputArray, inputArray, input)
      case _ =>
  end handleTargetImpl

  inline def handleTargetWithFImpl[F[_], From, To, ToValues <: Tuple, TLabels <: Tuple, AtPos <: Int](
    inline config: TypeDeriveConfig[_, _, _],
    fm: Mirror.ProductOf[From]
  )(
    outputArray: F[Array[Any]],
    inputArray: IArray[Any],
    input: From
  )(using TransformerFSupport[F]): F[Array[Any]] = 
    inline (erasedValue[ToValues], erasedValue[TLabels]) match
      case _: ((fieldType *: tailValues), (field *: tailLabels)) =>
        handleTargetWithFImpl[F, From, To, tailValues, tailLabels, AtPos + 1](
          config, 
          fm
        )(
          handleTargetFieldWithF[F, AtPos, field, fieldType, From, To](config, fm)(outputArray, inputArray, input), 
          inputArray, 
          input
        )
      case _ =>
        outputArray
  end handleTargetWithFImpl

  inline def handleTargetField[At <: Int, Name, T, From, To](
    inline config: TypeDeriveConfig[_, _, _],
    fm: Mirror.ProductOf[From]
  )(
    outputArray: Array[Any],
    inputArray: IArray[Any],
    input: From
  ): Unit = 
    inline config match
      case config: TypeDeriveConfig[config, flags, _] =>
        inline erasedValue[ConfigOf[config, Name]] match
          case _: FieldConst[_] => outputArray(constValue[At]) = config.overrides(constValue[Name].asInstanceOf)
          case _: FieldComputed[_] => 
            val f = config.overrides(constValue[Name].asInstanceOf).asInstanceOf[From => Any]
            outputArray(constValue[At]) = f(input)
          case _: FieldRelabelled[fieldFrom, Name] =>
            extractFromSource[At, fieldFrom, T, From, To, flags](config, fm)(outputArray, inputArray, input)
          case _ => 
            extractFromSource[At, Name, T, From, To, flags](config, fm)(outputArray, inputArray, input)
  end handleTargetField       

  inline def handleTargetFieldWithF[F[_], At <: Int, Name, T, From, To](
    inline config: TypeDeriveConfig[_, _, _],
    fm: Mirror.ProductOf[From]
  )(
    outputArray: F[Array[Any]],
    inputArray: IArray[Any],
    input: From
  )(using sup: TransformerFSupport[F]): F[Array[Any]] = 
    inline config match
      case config: TypeDeriveConfig[config, flags, _] =>
        inline erasedValue[ConfigOf[config, Name]] match
          case _: FieldConst[_] =>
            sup.map(outputArray, { outputArray =>
              outputArray(constValue[At]) = config.overrides(constValue[Name].asInstanceOf)
              outputArray
            })
          case _: FieldConstF[_] =>
            sup.map(
              sup.product(outputArray, config.overrides(constValue[Name].asInstanceOf).asInstanceOf[F[Any]]), { (outputArray, value) =>
                outputArray(constValue[At]) = value
                outputArray
              }
            )
          case _: FieldComputed[_] =>
            sup.map(outputArray, { outputArray =>
              val f = config.overrides(constValue[Name].asInstanceOf).asInstanceOf[From => Any]
              outputArray(constValue[At]) = f(input)
              outputArray
            })
          case _: FieldComputedF[_] =>
            val f = config.overrides(constValue[Name].asInstanceOf).asInstanceOf[From => F[Any]]
            sup.map(
              sup.product(outputArray, f(input)), { (outputArray, value) =>
                outputArray(constValue[At]) = value
                outputArray
              }  
            )
          case _: FieldRelabelled[fieldFrom, Name] =>
            sup.map(outputArray, outputArray =>
              extractFromSource[At, fieldFrom, T, From, To, flags](config, fm)(outputArray, inputArray, input)
              outputArray
            )
          case _ => 
            sup.map(
              sup.product(
                extractFromSourceF[F, At, Name, T, From, To, flags](config, fm)(outputArray, inputArray, input), 
                outputArray
              ),
              (_, outputArray) => outputArray
            )
            
  end handleTargetFieldWithF

  inline def extractFromSource[TargetAt <: Int, LabelAt, TypeAt, From, To, Flags <: Tuple](
    inline config: TypeDeriveConfig[_, Flags, _],
    fm: Mirror.ProductOf[From]
  )(outputArray: Array[Any], inputArray: IArray[Any], typedInput: From): Unit = 
    findInSource[From, To, LabelAt, fm.MirroredElemLabels, TypeAt, fm.MirroredElemTypes, 0](
      config)(outputArray, inputArray, typedInput, constValue[TargetAt])

  inline def extractFromSourceF[F[_]: TransformerFSupport, TargetAt <: Int, LabelAt, TypeAt, From, To, Flags <: Tuple](
    inline config: TypeDeriveConfig[_, Flags, _],
    fm: Mirror.ProductOf[From]
  )(outputArray: F[Array[Any]], inputArray: IArray[Any], typedInput: From): F[Unit] = 
    findInSourceWithF[F, From, To, LabelAt, fm.MirroredElemLabels, TypeAt, fm.MirroredElemTypes, 0](
      config)(outputArray, inputArray, typedInput, constValue[TargetAt])
    

  inline def findInSource[From, To, Field, SourceFields <: Tuple, Tpe, SourceTypes <: Tuple, Pos <: Int](
    inline config: TypeDeriveConfig[_, _, _])(outputArray: Array[Any], inputArray: IArray[Any], typedInput: From, targetPosition: Int): Unit = 
    inline config match
      case c: TypeDeriveConfig[_, flags, path] =>
        inline (erasedValue[SourceFields], erasedValue[SourceTypes]) match
          case _: (Field *: _, Tpe *: _) =>
            outputArray(targetPosition) = inputArray(constValue[Pos])
          case _: (Field *: _, tpe *: _) =>
            inline MacroUtils.attemptSummonInstance[Transformer[tpe, Tpe]] match
              case Some(transformer: Transformer[tpe, Tpe]) =>
                val fixed = transformer.asInstanceOf[Transformer[Any, Tpe]]
                outputArray(targetPosition) = fixed.transform(inputArray(constValue[Pos]))
              case _ =>
                outputArray(targetPosition) = TransformerDerive.deriveConfigured[tpe, Tpe, path Concat "." Concat Field](
                  configOfAtPath[Tpe, TransformerFlag.DefaultValues *: EmptyTuple, path Concat "." Concat Field](defaultDefinitionWithFlags))
                  .asInstanceOf[Transformer[Any, Tpe]]
                  .transform(inputArray(constValue[Pos]))
          case _: (_ *: sourceFields, _ *: sourceTypes) =>
            findInSource[From, To, Field, sourceFields, Tpe, sourceTypes, Pos + 1](config)(outputArray, inputArray, typedInput, targetPosition)
          case _: (EmptyTuple, _) =>
            specialExtractors[flags, path, From, To, Field, Tpe, Unit](c, typedInput) { extracted =>
              outputArray(targetPosition) = extracted
            }
  end findInSource

  inline def findInSourceWithF[F[_], From, To, Field, SourceFields <: Tuple, Tpe, SourceTypes <: Tuple, Pos <: Int](
    inline config: TypeDeriveConfig[_, _, _]
  )(
    outputArray: F[Array[Any]], 
    inputArray: IArray[Any], 
    typedInput: From,
    targetPosition: Int
  )(using sup: TransformerFSupport[F]): F[Unit] = 
    inline config match
      case c: TypeDeriveConfig[_, flags, path] =>
        inline (erasedValue[SourceFields], erasedValue[SourceTypes]) match
          case _: (Field *: _, Tpe *: _) =>
            sup.map(outputArray, outputArray => outputArray(targetPosition) = inputArray(constValue[Pos]))
          case _: (Field *: _, tpe *: _) =>
            summonFrom {
              case transformer: Transformer[tpe, Tpe] =>
                val fixed =  transformer.asInstanceOf[Transformer[Any, Tpe]]
                sup.map(outputArray, outputArray => 
                  outputArray(targetPosition) = fixed.transform(inputArray(constValue[Pos])))
              case transformerF: TransformerF[F, tpe, Tpe] =>
                val fixed = transformerF.asInstanceOf[TransformerF[F, Any, Tpe]]
                sup.map(
                  sup.product(outputArray, fixed.transform(inputArray(constValue[Pos]))),
                  (outputArray, value) => outputArray(targetPosition) = value
                )
              case _ =>
                sup.map(
                  outputArray,
                  outputArray => 
                  outputArray(targetPosition) = TransformerDerive.deriveConfigured[tpe, Tpe, Concat[path, path Concat "." Concat Field]](
                    configOfAtPath[Tpe, TransformerFlag.DefaultValues *: EmptyTuple, Concat[path, path Concat "." Concat Field]](defaultDefinitionWithFlags))
                    .asInstanceOf[Transformer[Any, Tpe]]
                    .transform(inputArray(constValue[Pos]))
                )
            }
          case _: (_ *: sourceFields, _ *: sourceTypes) =>
            findInSourceWithF[F, From, To, Field, sourceFields, Tpe, sourceTypes, Pos + 1](config)(outputArray, inputArray, typedInput, targetPosition)
          case _: (EmptyTuple, _) =>
            specialExtractors[flags, path, From, To, Field, Tpe, F[Unit]](c, typedInput) { extracted =>
              sup.map(outputArray, outputArray => outputArray(targetPosition) = extracted)
            }
  end findInSourceWithF

  private inline def specialExtractors[Flags <: Tuple, Path <: String, From, To, Field, Tpe, Res](inline config: TypeDeriveConfig[_, Flags, Path], from: From)(inline onAccess: Any => Res): Res =
    inline extractDefault[Flags, To, Field](config) match
      case Some(value) => onAccess(value)
      case None => 
        inline extractByMethod[Flags, From, Field](config, from) match
          case Some(value) => onAccess(value)
          case None => 
            inline if constValue[HasAFlag[Flags, TransformerFlag.OptionDefaultsToNone]] then
              inline erasedValue[Tpe] match
                case _: Option[?] =>
                  onAccess(None)
                case _ =>
                  error(constValue["Unable to find default value in target or method of a name (or those options are disabled), when deriving at: " Concat Path])    
            else
              error(constValue["Unable to find default value in target or method of a name (or those options are disabled), when deriving at: " Concat Path])
  end specialExtractors

  private transparent inline def extractDefault[Flags <: Tuple, To, Field](inline config: TypeDeriveConfig[_, Flags, _]) =
    inline if constValue[HasAFlag[Flags, TransformerFlag.DefaultValues]] then
      inline if MacroUtils.defaultValueExistsIn[To](constValue[Field]) then
        Some(config.defaults(constValue[Field].asInstanceOf))
      else None
    else None
  end extractDefault

  private transparent inline def extractByMethod[Flags <: Tuple, From, Field](inline config: TypeDeriveConfig[_, Flags, _], from: From) =
    inline if constValue[HasAFlag[Flags, TransformerFlag.MethodAccessors]] then
      ClassAcceessMacros.selectByName(from, constValue[Field])
    else 
      None
  end extractByMethod
  
end DeriveProduct

object DeriveUtils:

  final class TypeDeriveConfig[Config <: Tuple, Flags <: Tuple, Path <: String](
    val overrides: Map[String, Any],
    val defaults: Map[String, Any],
    val instances: Map[(String, String), Any]
  )

  inline def configOf[To, Config <: Tuple, Flags <: Tuple](
    definition: TransformerDefinition[_, To, Config, Flags]
  ): TypeDeriveConfig[Config, Flags, ""] =
    TypeDeriveConfig(definition.overrides, MacroUtils.getDefaultParams[To], definition.instances)

  inline def configOf[To, Config <: Tuple, Flags <: Tuple](
    definition: TransformerFDefinition[_, _, To, Config, Flags]
  ): TypeDeriveConfig[Config, Flags, ""] =
    TypeDeriveConfig(definition.overrides, MacroUtils.getDefaultParams[To], definition.instances)

  inline def configOfAtPath[To, Flags <: Tuple, Path <: String](
    definition: TransformerDefinition[_, To, EmptyTuple, Flags]
  ): TypeDeriveConfig[EmptyTuple, Flags, Path] =
    TypeDeriveConfig(definition.overrides, MacroUtils.getDefaultParams[To], definition.instances)

  import TransformerCfg._

  type ConfigOf[Config <: Tuple, Field] <: TransformerCfg = Config match
    case FieldConst[Field] *: _ => FieldConst[Field]
    case FieldConstF[Field] *: _ => FieldConstF[Field]
    case FieldComputed[Field] *: _ => FieldComputed[Field]
    case FieldComputedF[Field] *: _ => FieldComputedF[Field]
    case FieldRelabelled[fromField, Field] *: _ => FieldRelabelled[fromField, Field]
    case _ *: tail => ConfigOf[tail, Field]
    case EmptyTuple => TransformerCfg

  type InstanceConfigOf[Config <: Tuple, Instance] <: TransformerCfg = Config match
    case CoproductInstance[Instance, t] *: _ => CoproductInstance[Instance, t]
    case CoproductInstanceF[Instance, t] *: _ => CoproductInstanceF[Instance, t]
    case _ *: tail => InstanceConfigOf[tail, Instance]
    case EmptyTuple => TransformerCfg

  type HasAFlag[Labels <: Tuple, Flag <: TransformerFlag] <: Boolean = Labels match 
    case Flag *: _ => true
    case _ *: tail => HasAFlag[tail, Flag]
    case EmptyTuple => false

  import scala.compiletime.ops.string._

  type Concat[Path <: String, Field] <: String = Field match
    case String => Path + Field
    case _ => Path

  inline def transformerWith[From, To](inline impl: From => To): Transformer[From, To] = ${implementTransformerWith('impl)}

  inline def transformerWithF[F[_], From, To](inline impl: From => F[To]): TransformerF[F, From, To] = ${implementTransformerFWith('impl)}

  private def implementTransformerWith[From: Type, To: Type](impl: Expr[From => To])(using Quotes): Expr[Transformer[From, To]] =
    '{
      new TransformerImpl[From, To]:
        def transform(from: From): To = ${Expr.betaReduce('{(${impl})(from)})}
    }

  private def implementTransformerFWith[F[_]: Type, From: Type, To: Type](impl: Expr[From => F[To]])(using Quotes): Expr[TransformerF[F, From, To]] =
    '{
      new TransformerFImpl[F, From, To]:
        def transform(from: From): F[To] = ${Expr.betaReduce('{(${impl})(from)})}
    }

  abstract class TransformerImpl[From, To] extends Transformer[From, To]
  abstract class TransformerFImpl[F[_], From, To] extends TransformerF[F, From, To]

  inline def liftTransformer[F[_], From, To](t: Transformer[From, To])(using sup: TransformerFSupport[F]): TransformerF[F, From, To] =
    transformerWithF(from => sup.pure(t.transform(from)))

end DeriveUtils
