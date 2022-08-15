package io.scalaland.chimney.internal.derived

import io.scalaland.chimney.internal.TransformerFlag
import io.scalaland.chimney.internal.utils.modules.*
import io.scalaland.chimney.dsl.TransformerDefinition
import io.scalaland.chimney.*

import scala.deriving.*
import scala.quoted.*

trait ProductDeriveMacros:
  self: TransformerDeriveMacros & CommonDeriveModule & FieldModule & MirrorModule & ConfigModule & Module =>
  import quotes.reflect.*

  def deriveProduct[A: Type, B: Type, Flags <: Tuple: Type](
    source: Expr[A],
    materializedConfig: TransformerDefinitionMaterialized[Flags],
    sourceMirror: ProductMirror,
    targetMirror: ProductMirror
  ): Expr[B] = 
    val targetFields = TargetField.fromMirror(targetMirror)
    val regularSourceFields = ReadField.fromMirror(sourceMirror)
    val sourceFields = regularSourceFields ::: (if materializedConfig.hasAFlag[TransformerFlag.MethodAccessors] then methodFields(source, regularSourceFields, targetFields, identity) else Nil)
    val constructorArguments = deriveFromFields(source, materializedConfig, sourceFields, targetFields)
    constructor[B]
      .appliedToArgs(constructorArguments)
      .asExprOf[B]
  end deriveProduct

  def deriveProductFromJavaBean[A: Type, B: Type, Flags <: Tuple: Type](
    source: Expr[A],
    materializedConfig: TransformerDefinitionMaterialized[Flags],
    targetMirror: ProductMirror
  ): Expr[B] =
    val targetFields = TargetField.fromMirror(targetMirror)
    val sourceFields = methodFields(source, List.empty, targetFields, name => s"get${name.updated(0, name(0).toUpper)}")
    val constructorArguments = deriveFromFields(source, materializedConfig, sourceFields, targetFields)
    constructor[B]
      .appliedToArgs(constructorArguments)
      .asExprOf[B]
  end deriveProductFromJavaBean

  private def methodFields[A: Type](source: Expr[A], sourceFields: List[ReadField], targetFields: List[TargetField], convertName: String => String): List[ReadField] = 
    val knownFields = sourceFields.view.map(_.name).toSet
    val requiredFields = targetFields.filter(f => !knownFields.contains(f.name))
    if (requiredFields.nonEmpty)
      ReadField.fromTargetFields(source, convertName, requiredFields)
    else
      List.empty
  end methodFields

  private def constructor[B: Type]: Term = 
    val tpe = TypeRepr.of[B]
    val (repr, constructor, tpeArgs) = tpe match {
      case AppliedType(repr, reprArguments) => (repr, repr.typeSymbol.primaryConstructor, reprArguments)
      case notApplied                       => (tpe, tpe.typeSymbol.primaryConstructor, Nil)
    }

    New(Inferred(repr))
      .select(constructor)
      .appliedToTypes(tpeArgs)
  end constructor

end ProductDeriveMacros
