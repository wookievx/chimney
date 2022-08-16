package io.scalaland.chimney.internal.utils.modules

import io.scalaland.chimney.internal.{
  PatcherCfg,
  TransformerCfg,
  TransformerFlag
}
import io.scalaland.chimney.dsl.{TransformerDefinition, TransformerFDefinition}

import scala.collection.mutable
import scala.quoted.*

trait ConfigModule:
  self: Module =>
  import quotes.reflect.*

  case class TransformerDefinitionMaterialized[Flags <: Tuple](
    materializedConfig: List[MaterializedConfig],
    defaults: Option[Map[String, Term]]
  )

  object TransformerDefinitionMaterialized:
    def materialize[A: Type, Config <: Tuple: Type, Flags <: Tuple: Type](
      overrides: Expr[Array[Any]],
      instances: Expr[Array[Any]]
    ): TransformerDefinitionMaterialized[Flags] =
      TransformerDefinitionMaterialized(
        MaterializedConfig.materialize[Config](
          overrides,
          instances
        ),
        summonDefaults[A]
      )
  end TransformerDefinitionMaterialized

  case class TransformerFDefinitionMaterialized[Flags <: Tuple](
    materializedConfig: List[MaterializedConfig],
    defaults: Option[Map[String, Term]]
  )

  object TransformerFDefinitionMaterialized:
    def materialize[A: Type, Config <: Tuple: Type, Flags <: Tuple: Type](
      overrides: Expr[Array[Any]],
      instances: Expr[Array[Any]]
    ): TransformerFDefinitionMaterialized[Flags] =
      TransformerFDefinitionMaterialized(
        MaterializedConfig.materialize[Config](
          overrides,
          instances
        ),
        summonDefaults[A]
      )
  end TransformerFDefinitionMaterialized

  enum MaterializedConfig:
    case FieldConst(field: String, value: Expr[Any])
    case FieldConstF(field: String, value: Expr[Any])
    case FieldComputed(field: String, function: Expr[Any => Any])
    case FieldComputedF(field: String, function: Expr[Any => Any])
    case FieldRelabelled(fromField: String, field: String)
    case CoproductInstance(
      fromType: String,
      toType: String,
      function: Expr[Any => Any]
    )
    case CoproductInstanceF(
      fromType: String,
      toType: String,
      function: Expr[Any => Any]
    )
  end MaterializedConfig

  object MaterializedConfig:

    def materialize[Config <: Tuple: Type](
      overrides: Expr[Array[Any]],
      instances: Expr[Array[Any]]
    ): List[MaterializedConfig] =
      def implement[Config <: Tuple: Type](
        overrideIndex: Int,
        instancesIndex: Int
      ): List[MaterializedConfig] =
        Type.of[Config] match
          case '[TransformerCfg.FieldConst[field] *: tail] =>
            val field = getStringValueOf[field]
            FieldConst(
              field,
              '{ $overrides(${ Expr(overrideIndex) }) }
            ) :: implement[tail](overrideIndex + 1, instancesIndex)
          case '[TransformerCfg.FieldConstF[field] *: tail] =>
            val field = getStringValueOf[field]
            FieldConstF(
              field,
              '{ $overrides(${ Expr(overrideIndex) }) }
            ) :: implement[tail](overrideIndex + 1, instancesIndex)
          case '[TransformerCfg.FieldComputed[field] *: tail] =>
            val field = getStringValueOf[field]
            FieldComputed(
              field,
              '{ $overrides(${ Expr(overrideIndex) }).asInstanceOf[Any => Any] }
            ) :: implement[tail](overrideIndex + 1, instancesIndex)
          case '[TransformerCfg.FieldComputedF[field] *: tail] =>
            val field = getStringValueOf[field]
            FieldComputedF(
              field,
              '{ $overrides(${ Expr(overrideIndex) }).asInstanceOf[Any => Any] }
            ) :: implement[tail](overrideIndex + 1, instancesIndex)
          case '[TransformerCfg.FieldRelabelled[fromField, field] *: tail] =>
            val fromField = getStringValueOf[fromField]
            val toField = getStringValueOf[field]
            FieldRelabelled(fromField, toField) :: implement[tail](
              overrideIndex,
              instancesIndex
            )
          case '[TransformerCfg.CoproductInstance[from, to] *: tail] =>
            val from = showType[from]
            val to = showType[to]
            CoproductInstance(
              from,
              to,
              '{ $instances(${ Expr(instancesIndex) }).asInstanceOf[Any => Any] }
            ) :: implement[tail](overrideIndex, instancesIndex + 1)
          case '[TransformerCfg.CoproductInstanceF[from, to] *: tail] =>
            val from = showType[from]
            val to = showType[to]
            CoproductInstanceF(
              from,
              to,
              '{ $instances(${ Expr(instancesIndex) }).asInstanceOf[Any => Any] }
            ) :: implement[tail](overrideIndex, instancesIndex + 1)
          case '[_ *: tail] =>
            implement[tail](overrideIndex, instancesIndex)
          case _ =>
            Nil
      end implement

      implement[Config](0, 0)
    end materialize

  end MaterializedConfig

  extension (definition: TransformerDefinitionMaterialized[?])
    def isOverriden(field: String): Option[Expr[Any]] =
      definition.materializedConfig.collectFirst {
        case MaterializedConfig.FieldConst(`field`, value) => value
      }
    def isComputed(field: String): Option[Expr[Any => Any]] =
      definition.materializedConfig.collectFirst {
        case MaterializedConfig.FieldComputed(`field`, func) => func
      }
    def isRenamed(field: String): Option[String] =
      definition.materializedConfig.collectFirst {
        case MaterializedConfig.FieldRelabelled(from, `field`) => from
      }
    def isCaseComputed(caseName: String): Option[Expr[Any => Any]] =
      definition.materializedConfig.collectFirst {
        case MaterializedConfig.CoproductInstance(`caseName`, _, func) => func
        case MaterializedConfig.CoproductInstance(name, _, func)
            if name.endsWith(s".$caseName") =>
          func
      }

  extension [Flags <: Tuple: Type](
    definition: TransformerDefinitionMaterialized[Flags]
  )
    def hasAFlag[Flag <: TransformerFlag: Type]: Boolean =
      hasAFlagImpl[Flags, Flag]
    def inherited[A: Type]: TransformerDefinitionMaterialized[Flags] =
      TransformerDefinitionMaterialized(
        List.empty,
        summonDefaults[A]
      )

  extension (definition: TransformerFDefinitionMaterialized[?])
    def isOverriden(field: String): Option[Expr[Any]] =
      definition.materializedConfig.collectFirst {
        case MaterializedConfig.FieldConst(`field`, value) => value
      }
    def isOverridenF(field: String): Option[Expr[Any]] =
      definition.materializedConfig.collectFirst {
        case MaterializedConfig.FieldConstF(`field`, value) => value
      }
    def isComputed(field: String): Option[Expr[Any => Any]] =
      definition.materializedConfig.collectFirst {
        case MaterializedConfig.FieldComputed(`field`, func) => func
      }
    def isComputedF(field: String): Option[Expr[Any => Any]] =
      definition.materializedConfig.collectFirst {
        case MaterializedConfig.FieldComputedF(`field`, func) => func
      }
    def isRenamed(field: String): Option[String] =
      definition.materializedConfig.collectFirst {
        case MaterializedConfig.FieldRelabelled(from, `field`) => from
      }
    def isCaseComputed(caseName: String): Option[Expr[Any => Any]] =
      definition.materializedConfig.collectFirst {
        case MaterializedConfig.CoproductInstance(`caseName`, _, func) => func
        case MaterializedConfig.CoproductInstance(name, _, func)
            if name.endsWith(s".$caseName") =>
          func
      }
    def isCaseComputedF(caseName: String): Option[Expr[Any => Any]] =
      definition.materializedConfig.collectFirst {
        case MaterializedConfig.CoproductInstanceF(`caseName`, _, func) => func
        case MaterializedConfig.CoproductInstanceF(name, _, func)
            if name.endsWith(s".$caseName") =>
          func
      }

  extension [Flags <: Tuple: Type](
    definition: TransformerFDefinitionMaterialized[Flags]
  )
    def hasAFlag[Flag <: TransformerFlag: Type]: Boolean =
      hasAFlagImpl[Flags, Flag]
    def inherited[A: Type]: TransformerDefinitionMaterialized[Flags] =
      TransformerDefinitionMaterialized(
        List.empty,
        summonDefaults[A]
      )

  case class OptimizedConfig(
    overrides: Expr[Array[Any]],
    instances: Expr[Array[Any]]
  )

  object OptimizedConfig:
    //converts maps to arrays for easy index access, should be sufficient optimization and balance between optimization
    //and generated code quality
    def optimizedConfig[Config <: Tuple: Type](
      definition: Expr[TransformerDefinition[?, ?, Config, ?]]
    ): OptimizedConfig =
      val overrides = '{ $definition.overrides }
      val instances = '{ $definition.instances }
      def implement[Config <: Tuple: Type](
        overridesBuild: Expr[mutable.ArrayBuilder[Any]],
        instancesBuild: Expr[mutable.ArrayBuilder[Any]]
      ): (Expr[mutable.ArrayBuilder[Any]], Expr[mutable.ArrayBuilder[Any]]) =
        Type.of[Config] match
          case '[TransformerCfg.FieldConst[field] *: tail] =>
            val field = getStringValueOf[field]
            val newOverrides = '{
              $overridesBuild += $overrides(${ Expr(field) })
            }
            implement[tail](newOverrides, instancesBuild)
          case '[TransformerCfg.FieldConstF[field] *: tail] =>
            val field = getStringValueOf[field]
            val newOverrides = '{
              $overridesBuild += $overrides(${ Expr(field) })
            }
            implement[tail](newOverrides, instancesBuild)
          case '[TransformerCfg.FieldComputed[field] *: tail] =>
            val field = getStringValueOf[field]
            val newOverrides = '{
              $overridesBuild += $overrides(${ Expr(field) })
            }
            implement[tail](newOverrides, instancesBuild)
          case '[TransformerCfg.FieldComputedF[field] *: tail] =>
            val field = getStringValueOf[field]
            val newOverrides = '{
              $overridesBuild += $overrides(${ Expr(field) })
            }
            implement[tail](newOverrides, instancesBuild)
          case '[TransformerCfg.CoproductInstance[from, to] *: tail] =>
            val from = showType[from]
            val to = showType[to]
            val newInstances = '{
              $instancesBuild += $instances(${ Expr(from) }, ${ Expr(to) })
            }
            implement[tail](overridesBuild, newInstances)
          case '[TransformerCfg.CoproductInstanceF[from, to] *: tail] =>
            val from = showType[from]
            val to = showType[to]
            val newInstances = '{
              $instancesBuild += $instances(${ Expr(from) }, ${ Expr(to) })
            }
            implement[tail](overridesBuild, newInstances)
          case '[_ *: tail] =>
            implement[tail](overridesBuild, instancesBuild)
          case _ =>
            (overridesBuild, instancesBuild)
      end implement

      val (overridesBuilder, instancesBuilder) = implement[Config](
        '{ Array.newBuilder[Any] },
        '{ Array.newBuilder[Any] }
      )
      OptimizedConfig(
        overrides = '{ $overridesBuilder.result },
        instances = '{ $instancesBuilder.result }
      )
    end optimizedConfig
  end OptimizedConfig

  private def hasAFlagImpl[Flags <: Tuple: Type, Flag <: TransformerFlag: Type]
    : Boolean =
    Type.of[Flags] match
      case '[Flag *: _]  => true
      case '[_ *: flags] => hasAFlagImpl[flags, Flag]
      case _             => false

  private def getStringValueOf[Field: Type]: String =
    Type.valueOfConstant[Field] match
      case Some(field: String) => field
      case f =>
        report.errorAndAbort(s"Illegal value in config: $f, should not happen")

  private def summonDefaults[A: Type]: Option[Map[String, Term]] =
    val symbol = TypeTree.of[A].symbol
    if (symbol.isClassDef) {
      val companion = symbol.companionClass
      val classDefinition: Option[ClassDef] =
        try {
          Some(companion.tree.asInstanceOf[ClassDef])
        } catch {
          case _ => None
        }
      classDefinition.map { classDef =>
        val body = classDef.body
        val defaultParams =
          for
            case deff @ DefDef(name, _, _, _) <- body.view
            if name.startsWith("$lessinit$greater$default")
          yield (name, Ref(deff.symbol))

        defaultParams.toMap
      }
    } else {
      None
    }

  def showType[T: Type]: String = Type.show[T]

end ConfigModule
