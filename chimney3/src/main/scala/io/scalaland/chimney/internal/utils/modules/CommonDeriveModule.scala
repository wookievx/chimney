package io.scalaland.chimney.internal.utils.modules

import io.scalaland.chimney.internal.TransformerFlag
import io.scalaland.chimney.internal.derived.TransformerDeriveMacros
import io.scalaland.chimney.{Transformer, TransformerF}

import scala.quoted.*
import scala.util.chaining.*

//this module encapsulates all the logic regarding custom field logic, aiming to provide similar behavior for products and java beans
trait CommonDeriveModule:
  self: TransformerDeriveMacros & FieldModule & MirrorModule & ConfigModule &
    Module =>
  import quotes.reflect.*

  type ConstructionArguments = List[Term]

  def deriveFromFields[A: Type, Flags <: Tuple: Type](
    source: Expr[A],
    config: TransformerDefinitionMaterialized[Flags],
    sourceFields: List[ReadField],
    targetFields: List[TargetField]
  ): ConstructionArguments =
    val defaults =
      if config.hasAFlag[TransformerFlag.DefaultValues] then
        config.defaults.map(defaults =>
          defaults.view
            .map((name, value) => name -> NamedArg(name, value))
            .toList
        )
      else None
      end if
    val (regularFields, regularValues) =
      matchTargetWithSource(source, config, sourceFields, targetFields).unzip

    val (defaultsNames, defaultList) = defaults.getOrElse(List.empty).unzip
    if verifyAllFieldsSet(regularFields, defaultsNames, targetFields) then
      regularValues ::: defaultList
    else report.errorAndAbort(s"Missing value for some fields in target")
    end if
  end deriveFromFields

  def deriveFromFieldsF[F[_]: Type, A: Type, Flags <: Tuple: Type](
    source: Expr[A],
    config: TransformerDefinitionMaterialized[Flags],
    sourceFields: List[ReadField],
    targetFields: List[TargetField]
  ): ConstructionArguments = ???

  def elemWiseTransform[A: Type, B: Type, Flags <: Tuple: Type](
    from: Expr[A],
    config: TransformerDefinitionMaterialized[Flags]
  ): Expr[B] =
    if TypeRepr.of[A] <:< TypeRepr.of[B] then
      from.asInstanceOf[Expr[B]] //this cast is safe, no flow typing :(
    else deriveTransformer[A, B, Flags](from, config.inherited[A])
    end if
  end elemWiseTransform

  private def matchTargetWithSource[A: Type, Flags <: Tuple: Type](
    source: Expr[A],
    config: TransformerDefinitionMaterialized[Flags],
    sourceFields: List[ReadField],
    targetFields: List[TargetField]
  ): List[(String, Term)] =
    val sourceMap = sourceFields.view.map(f => f.name -> f).toMap
    val fieldValues: Map[TargetField, Expr[?]] = targetFields.view.flatMap {
      field =>
        val appliedExpr: Option[Expr[?]] =
          config.isOverriden(field.name).map(castValueToFieldType(field)) orElse
            config
              .isComputed(field.name)
              .map(castComputedValueToFieldType(field, source)) orElse
            config
              .isRenamed(field.name)
              .getOrElse(field.name)
              .pipe(sourceMap.get)
              .map(sourceField =>
                accessField[A, Flags](source, config, sourceField, field)
              )
        appliedExpr.map(field -> _)
    }.toMap
    fieldValues.view.map { (targetField, value) =>
      value match
        case '{ $value: b } =>
          targetField.name -> targetField.setValue(value)
    }.toList
  end matchTargetWithSource

  private def castValueToFieldType(field: TargetField)(
    value: Expr[Any]
  ): Expr[?] =
    field.tpe.asType match
      case '[t] => '{ $value.asInstanceOf[t] }

  private def castComputedValueToFieldType(field: TargetField, source: Expr[?])(
    func: Expr[Any => Any]
  ): Expr[?] =
    field.tpe.asType match
      case '[t] => '{ $func($source).asInstanceOf[t] }

  private def verifyAllFieldsSet(
    regularFields: List[String],
    defaults: List[String],
    targetFields: List[TargetField]
  ): Boolean =
    val fieldsSet = regularFields.toSet ++ defaults
    val isCorrect = targetFields.forall(f =>
      fieldsSet.contains(f.name) || {
        report.error(s"Missing value for field $f"); false
      }
    )
    isCorrect
  end verifyAllFieldsSet

  private def accessField[A: Type, Flags <: Tuple: Type](
    value: Expr[A],
    parentConfig: TransformerDefinitionMaterialized[Flags],
    fieldFrom: ReadField,
    fieldTo: TargetField
  ): Expr[?] =
    if fieldFrom.tpe <:< fieldTo.tpe then fieldFrom.accessFrom(value)
    else
      (fieldFrom.accessFrom(value), fieldTo.tpe.asType) match
        case ('{ $v: a }, '[b]) =>
          Expr.summon[Transformer[a, b]] match
            case Some(transformer) =>
              '{ $transformer.transform($v) }
            case None =>
              deriveTransformer[a, b, Flags](v, parentConfig.inherited[a])
    end if
  end accessField

end CommonDeriveModule
