package io.scalaland.chimney.internal.utils.modules

import io.scalaland.chimney.internal.TransformerFlag
import io.scalaland.chimney.internal.derived.TransformerDeriveMacros
import io.scalaland.chimney.internal.utils.{MacroUtils, modules}
import io.scalaland.chimney.{Transformer, TransformerF, TransformerFSupport}

import scala.collection.mutable
import scala.quoted.*
import scala.util.chaining.*

//this module encapsulates all the logic regarding custom field logic, aiming to provide similar behavior for products and java beans
trait CommonDeriveModule:
  self: TransformerDeriveMacros & FieldModule & MirrorModule & ConfigModule &
    Module =>
  import quotes.reflect.*

  type ConstructionArguments = List[Term]
  type FConstructionArguments[F[_]] =
    (List[TargetField], Option[Expr[F[NonEmptyTuple]]])

  def deriveFromFields[A: Type, Flags <: Tuple: Type](
    source: Expr[A],
    config: TransformerDefinitionMaterialized[Flags],
    sourceFields: List[ReadField],
    targetFields: List[TargetField]
  ): ConstructionArguments =
    val (regularFields, regularValues) =
      matchTargetWithSource(source, config, sourceFields, targetFields).unzip

    val defaults = notProvidedDefaults(config, regularFields.toSet)

    val (defaultsNames, defaultList) = defaults.getOrElse(List.empty).unzip
    if verifyAllFieldsSet(regularFields, defaultsNames, targetFields) then
      regularValues ::: defaultList
    else report.errorAndAbort(s"Missing value for some fields in target")
    end if
  end deriveFromFields

  def computeFieldValues[F[_]: Type, A: Type, Flags <: Tuple: Type](
    source: Expr[A],
    config: TransformerFDefinitionMaterialized[Flags],
    support: Expr[TransformerFSupport[F]],
    sourceFields: List[ReadField],
    targetFields: List[TargetField]
  ): FConstructionArguments[F] =
    def constructBody(
      first: Expr[F[Any]],
      left: List[Expr[F[Any]]]
    ): Expr[F[NonEmptyTuple]] =
      left match
        case head :: tail =>
          '{
            $support.map(
              $support.product($first, ${ constructBody(head, tail) }),
              _ *: _
            )
          }
        case Nil =>
          '{
            $support.map(
              $support.product($first, $support.pure(EmptyTuple)),
              _ *: _
            )
          }
    end constructBody
    val (fields, fieldComputed) = matchTargetWithSource[F, A, Flags](
      source,
      config,
      support,
      sourceFields,
      targetFields
    ).unzip

    (
      fields,
      fieldComputed match
        case h :: tail =>
          Some(constructBody(h, tail))
        case Nil =>
          None
    )
  end computeFieldValues

  def deriveFromComputedFields[Flags <: Tuple: Type](
    definedFields: List[TargetField],
    computedSource: Expr[NonEmptyTuple],
    config: TransformerFDefinitionMaterialized[Flags],
    targetFields: List[TargetField]
  ): ConstructionArguments =
    val constValueFields =
      definedFields.view.zipWithIndex
        .map((field, index) =>
          field.setValueUnsafe('{
            $computedSource.productElement(${ Expr(index) })
          })
        )
        .toList

    val defaults = notProvidedDefaultsF(config, definedFields.view.map(_.name).toSet)

    val (defaultsNames, defaultList) = defaults.getOrElse(List.empty).unzip
    if verifyAllFieldsSet(
        definedFields.map(_.name),
        defaultsNames,
        targetFields
      )
    then constValueFields ::: defaultList
    else report.errorAndAbort(s"Missing value for some fields in target")
    end if

  end deriveFromComputedFields

  def deriveFromOnlyDefaults[Flags <: Tuple: Type](
    config: TransformerFDefinitionMaterialized[Flags],
    targetFields: List[TargetField]
  ) =
    val defaults = notProvidedDefaultsF(config, Set.empty)

    val (defaultsNames, defaultList) = defaults.getOrElse(List.empty).unzip
    if verifyAllFieldsSet(List.empty, defaultsNames, targetFields) then
      defaultList
    else report.errorAndAbort(s"Missing value for some fields in target")
    end if
  end deriveFromOnlyDefaults

  def elemWiseTransform[A: Type, B: Type, Flags <: Tuple: Type](
    from: Expr[A],
    config: TransformerDefinitionMaterialized[Flags]
  ): Expr[B] =
    if TypeRepr.of[A] <:< TypeRepr.of[B] then
      from.asInstanceOf[Expr[B]] //this cast is safe, no flow typing :(
    else
      Expr.summon[Transformer[A, B]] match
        case Some(transformer) =>
          '{ $transformer.transform($from) }
        case None =>
          deriveTransformer[A, B, Flags](from, config.inherited[A])
    end if
  end elemWiseTransform

  def elemWiseTransformF[F[_]: Type, A: Type, B: Type, Flags <: Tuple: Type](
    from: Expr[A],
    config: TransformerFDefinitionMaterialized[Flags],
    support: Expr[TransformerFSupport[F]]
  ): Expr[F[B]] =
    if TypeRepr.of[A] <:< TypeRepr.of[B] then
      '{ $support.pure($from.asInstanceOf[B]) }
    else
      val transformerRes = Expr
        .summon[Transformer[A, B]]
        .map(transformer => '{ $support.pure($transformer.transform($from)) })
      val transformerFRes = Expr
        .summon[TransformerF[F, A, B]]
        .map(transformer => '{ $transformer.transform($from) })
      transformerRes orElse transformerFRes getOrElse deriveTransformerF[
        F,
        A,
        B,
        Flags
      ](from, config.inherited[A], support)
    end if
  end elemWiseTransformF

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

  private def matchTargetWithSource[F[_]: Type, A: Type, Flags <: Tuple: Type](
    source: Expr[A],
    config: TransformerFDefinitionMaterialized[Flags],
    support: Expr[TransformerFSupport[F]],
    sourceFields: List[ReadField],
    targetFields: List[TargetField]
  ): List[(TargetField, Expr[F[Any]])] =
    val sourceMap = sourceFields.view.map(f => f.name -> f).toMap
    targetFields.flatMap { field =>
      val appliedExpr: Option[Expr[F[Any]]] =
        config
          .isOverriden(field.name)
          .map(castValueToFieldType(field))
          .map(liftAnyToF(support)) orElse
          config.isOverridenF(field.name).map(castValueToAnyF) orElse
          config
            .isComputed(field.name)
            .map(castComputedValueToFieldType(field, source))
            .map(liftAnyToF(support)) orElse
          config
            .isComputedF(field.name)
            .map(castComputedValueToF(source)) orElse
          config
            .isRenamed(field.name)
            .getOrElse(field.name)
            .pipe(sourceMap.get)
            .map(sourceField =>
              accessFieldF[F, A, Flags](
                source,
                config,
                support,
                sourceField,
                field
              ) match
                case '{ $f: F[a] } => '{ $f.asInstanceOf[F[Any]] }
                case _ =>
                  report.errorAndAbort(
                    s"Encountered unexpected expression in macro, should not happen"
                  )
            )
      appliedExpr.map(field -> _)
    }
  end matchTargetWithSource

  private def notProvidedDefaults[Flags <: Tuple: Type](
    config: TransformerDefinitionMaterialized[Flags],
    providedFields: Set[String]
  ): Option[List[(String, Term)]] =
    if config.hasAFlag[TransformerFlag.DefaultValues] then
      config.defaults.map(defaults =>
        defaults.view
          .filterNot((name, _) => providedFields.contains(name))
          .map((name, value) => name -> NamedArg(name, value))
          .toList
      )
    else None
    end if
  end notProvidedDefaults

  private def notProvidedDefaultsF[Flags <: Tuple: Type](
    config: TransformerFDefinitionMaterialized[Flags],
    providedFields: Set[String]
  ): Option[List[(String, Term)]] =
    if config.hasAFlag[TransformerFlag.DefaultValues] then
      config.defaults.map(defaults =>
        defaults.view
          .filterNot((name, _) => providedFields.contains(name))
          .map((name, value) => name -> NamedArg(name, value))
          .toList
      )
    else None
    end if
  end notProvidedDefaultsF

  private def castValueToFieldType(field: TargetField)(
    value: Expr[Any]
  ): Expr[?] =
    field.tpe.asType match
      case '[t] => '{ $value.asInstanceOf[t] }

  private def castValueToAnyF[F[_]: Type](
    value: Expr[Any]
  ): Expr[F[Any]] = '{ $value.asInstanceOf[F[Any]] }

  private def liftAnyToF[F[_]: Type](support: Expr[TransformerFSupport[F]])(
    value: Expr[Any]
  ): Expr[F[Any]] =
    '{ $support.pure($value) }

  private def castComputedValueToFieldType(field: TargetField, source: Expr[?])(
    func: Expr[Any => Any]
  ): Expr[?] =
    field.tpe.asType match
      case '[t] => '{ $func($source).asInstanceOf[t] }

  private def castComputedValueToF[F[_]: Type](source: Expr[?])(
    func: Expr[Any => Any]
  ): Expr[F[Any]] =
    '{ $func($source).asInstanceOf[F[Any]] }

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

  private def accessFieldF[F[_]: Type, A: Type, Flags <: Tuple: Type](
    value: Expr[A],
    parentConfig: TransformerFDefinitionMaterialized[Flags],
    support: Expr[TransformerFSupport[F]],
    fieldFrom: ReadField,
    fieldTo: TargetField
  ): Expr[?] =
    (fieldFrom.tpe.asType, fieldTo.tpe.asType) match
      case ('[a], '[b]) =>
        elemWiseTransformF[F, a, b, Flags](
          '{ ${ fieldFrom.accessFrom(value) }.asInstanceOf[a] },
          parentConfig,
          support
        )
  end accessFieldF

  private def accessField[A: Type, Flags <: Tuple: Type](
    value: Expr[A],
    parentConfig: TransformerDefinitionMaterialized[Flags],
    fieldFrom: ReadField,
    fieldTo: TargetField
  ): Expr[?] =
    (fieldFrom.tpe.asType, fieldTo.tpe.asType) match
      case ('[a], '[b]) =>
        elemWiseTransform[a, b, Flags](
          '{ ${ fieldFrom.accessFrom(value) }.asInstanceOf[a] },
          parentConfig
        )
  end accessField

end CommonDeriveModule
