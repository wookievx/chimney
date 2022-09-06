package io.scalaland.chimney.internal.derived

import io.scalaland.chimney.internal.TransformerFlag
import io.scalaland.chimney.internal.utils.modules.*
import io.scalaland.chimney.dsl.TransformerDefinition
import io.scalaland.chimney.*

import scala.deriving.*
import scala.quoted.*

trait ProductDeriveMacros:
  self: TransformerDeriveMacros & CommonDeriveModule & FieldModule &
    MirrorModule & ConfigModule & Module =>
  import quotes.reflect.*

  def deriveProduct[A: Type, B: Type, Flags <: Tuple: Type](
    source: Expr[A],
    materializedConfig: TransformerDefinitionMaterialized[Flags],
    sourceMirror: ProductMirror,
    targetMirror: ProductMirror
  ): Expr[B] =
    if targetMirror.fields.isEmpty then summonSingleton[B]
    else
      val targetFields = TargetField.fromMirror(targetMirror)
      val regularSourceFields = ReadField.fromMirror(sourceMirror)
      val sourceFields =
        regularSourceFields ::: (if materializedConfig
                                     .hasAFlag[TransformerFlag.MethodAccessors]
                                 then
                                   methodFields(
                                     source,
                                     regularSourceFields,
                                     targetFields,
                                     identity
                                   )
                                 else Nil)
      val constructorArguments =
        deriveFromFields(source, materializedConfig, sourceFields, targetFields)
      constructor[B]
        .appliedToArgs(constructorArguments)
        .asExprOf[B]
    end if
  end deriveProduct

  def deriveProductF[F[_]: Type, A: Type, B: Type, Flags <: Tuple: Type](
    source: Expr[A],
    materializedConfig: TransformerFDefinitionMaterialized[Flags],
    support: Expr[TransformerFSupport[F]],
    sourceMirror: ProductMirror,
    targetMirror: ProductMirror
  ): Expr[F[B]] =
    if targetMirror.fields.isEmpty then '{$support.pure(${summonSingleton[B]})}
    else
      val targetFields = TargetField.fromMirror(targetMirror)
      val regularSourceFields = ReadField.fromMirror(sourceMirror)
      val sourceFields =
        regularSourceFields ::: (if materializedConfig
                                     .hasAFlag[TransformerFlag.MethodAccessors]
                                 then
                                   methodFields(
                                     source,
                                     regularSourceFields,
                                     targetFields,
                                     identity
                                   )
                                 else Nil)
      val (computedFieldsNames, computedFields) = computeFieldValues(
        source,
        materializedConfig,
        support,
        sourceFields,
        targetFields
      )
      computedFields match
        case Some(computedFields) =>
          '{
            $support.map(
              $computedFields,
              fields =>
                ${
                  constructor[B]
                    .appliedToArgs(
                      deriveFromComputedFields(
                        computedFieldsNames,
                        '{ fields },
                        materializedConfig,
                        targetFields
                      )
                    )
                    .asExprOf[B]
                }
            )
          }
        case None =>
          '{
            $support.pure(${
              constructor[B]
                .appliedToArgs(
                  deriveFromOnlyDefaults(materializedConfig, targetFields)
                )
                .asExprOf[B]
            })
          }
    end if
  end deriveProductF

  def deriveProductFromJavaBean[A: Type, B: Type, Flags <: Tuple: Type](
    source: Expr[A],
    materializedConfig: TransformerDefinitionMaterialized[Flags],
    targetMirror: ProductMirror
  ): Expr[B] =
    if targetMirror.fields.isEmpty then summonSingleton[B]
    else
      val targetFields = TargetField.fromMirror(targetMirror)
      val sourceFields = methodFields(
        source,
        List.empty,
        targetFields,
        name => s"get${name.updated(0, name(0).toUpper)}"
      )
      val constructorArguments =
        deriveFromFields(source, materializedConfig, sourceFields, targetFields)
      constructor[B]
        .appliedToArgs(constructorArguments)
        .asExprOf[B]
    end if
  end deriveProductFromJavaBean

  def deriveProductFromJavaBeanF[
    F[_]: Type,
    A: Type,
    B: Type,
    Flags <: Tuple: Type
  ](
    source: Expr[A],
    materializedConfig: TransformerFDefinitionMaterialized[Flags],
    support: Expr[TransformerFSupport[F]],
    targetMirror: ProductMirror
  ): Expr[F[B]] =
    if targetMirror.fields.isEmpty then '{$support.pure(${summonSingleton[B]})}
    else
      val targetFields = TargetField.fromMirror(targetMirror)
      val sourceFields = methodFields(
        source,
        List.empty,
        targetFields,
        name => s"get${name.updated(0, name(0).toUpper)}"
      )
      val (computedFieldsNames, computedFields) = computeFieldValues(
        source,
        materializedConfig,
        support,
        sourceFields,
        targetFields
      )
      computedFields match
        case Some(computedFields) =>
          '{
            $support.map(
              $computedFields,
              fields =>
                ${
                  constructor[B]
                    .appliedToArgs(
                      deriveFromComputedFields(
                        computedFieldsNames,
                        '{ fields },
                        materializedConfig,
                        targetFields
                      )
                    )
                    .asExprOf[B]
                }
            )
          }
        case None =>
          '{
            $support.pure(${
              constructor[B]
                .appliedToArgs(
                  deriveFromOnlyDefaults(materializedConfig, targetFields)
                )
                .asExprOf[B]
            })
          }
    end if
  end deriveProductFromJavaBeanF

  private def methodFields[A: Type](
    source: Expr[A],
    sourceFields: List[ReadField],
    targetFields: List[TargetField],
    convertName: String => String
  ): List[ReadField] =
    val knownFields = sourceFields.view.map(_.name).toSet
    val requiredFields = targetFields.filter(f => !knownFields.contains(f.name))
    if (requiredFields.nonEmpty)
      ReadField.fromTargetFields(source, convertName, requiredFields)
    else
      List.empty
  end methodFields

  private def summonSingleton[B: Type]: Expr[B] =
    val tpe = TypeRepr.of[B]
    if tpe.isSingleton then
      tpe match {
        case TermRef(a, b) => Ident(TermRef(a, b)).asExprOf[B]
      }
    else
      report.errorAndAbort(s"Probably a bug in library, received empty mirror for non-singleton: ${Type.show[B]}")
  end summonSingleton


end ProductDeriveMacros
