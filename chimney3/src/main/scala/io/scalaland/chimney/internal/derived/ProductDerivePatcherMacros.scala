package io.scalaland.chimney.internal.derived

import io.scalaland.chimney.Patcher
import io.scalaland.chimney.internal.PatcherCfg
import io.scalaland.chimney.internal.utils.modules.*

import scala.quoted.*

trait ProductDerivePatcherMacros:
  self: PatcherDeriveMacros & FieldModule & MirrorModule & Module =>
  import quotes.reflect.*

  def deriveProduct[A: Type, B: Type, Config <: Tuple: Type](
    path: String,
    sourceMirror: ProductMirror,
    patchMirror: ProductMirror
  ): Expr[Patcher[A, B]] =
    val sourceFields =
      ReadField.fromMirror(sourceMirror).view.map(f => f.name -> f).toMap
    val targetFields =
      TargetField.fromMirror(sourceMirror).view.map(f => f.name -> f).toMap

    val patchFields =
      ReadField.fromMirror(patchMirror).view.map(f => f.name -> f).toMap

    '{
      new Patcher[A, B]:
        override def patch(obj: A, patch: B): A =
          ${
            val appliedPerformPatch = performPatchOpt[A, B, Config](
              '{ obj },
              '{ patch },
              path
            )
            val fields = sourceFields.view.map { case (name, field) =>
              appliedPerformPatch(
                field,
                patchFields.get(name),
                targetFields(name)
              )
            }.toList
            constructor[A]
              .appliedToArgs(
                fields
              )
              .asExprOf[A]
          }
    }

  end deriveProduct

  private def performPatchOpt[A: Type, B: Type, Config <: Tuple: Type](
    sourceValue: Expr[A],
    patchValue: Expr[B],
    path: String
  )(
    sourceFieldDef: ReadField,
    patchFieldDef: Option[ReadField],
    targetFieldDef: TargetField
  ): Term =
    patchFieldDef match
      case Some(patchFieldDef) =>
        performPatch[A, B, Config](sourceValue, patchValue, path)(
          sourceFieldDef,
          patchFieldDef,
          targetFieldDef
        )
      case None =>
        targetFieldDef.setValue(sourceFieldDef.accessFrom(sourceValue))
  end performPatchOpt

  private def performPatch[A: Type, B: Type, Config <: Tuple: Type](
    sourceValue: Expr[A],
    patchValue: Expr[B],
    path: String
  )(
    sourceFieldDef: ReadField,
    patchFieldDef: ReadField,
    targetFieldDef: TargetField
  ): Term =
    (sourceFieldDef.tpe.asType, patchFieldDef.tpe.asType) match
      case ('[a], '[b]) =>
        val patcher = patchField[a, b, Config](path, sourceFieldDef.name)
        val patchFieldValue = patchFieldDef.accessFrom(patchValue).asExprOf[b]
        val patchOperation = sourceFieldDef.patchWithValue[A, a, b](
          sourceValue,
          patchFieldValue,
          patcher
        )
        targetFieldDef.setValue(patchOperation)
  end performPatch

  private def patchField[A: Type, B: Type, Config <: Tuple: Type](
    path: String,
    field: String
  ): Expr[Patcher[A, B]] =
    Type.of[B] match
      case '[Option[b]] =>
        Type.of[A] match
          case '[Option[a]] =>
            val underlying = patchField[a, b, Config](path, field)
            '{
              new Patcher[A, B]:
                override def patch(obj: A, patch: B): A =
                  val optPatch = for {
                    obj <- obj.asInstanceOf[Option[a]]
                    patch <- patch.asInstanceOf[Option[b]]
                  } yield $underlying.patch(obj, patch)

                  ${
                    if hasPatcherConfig[
                        Config,
                        PatcherCfg.IgnoreNoneInPatch
                      ]
                    then
                      '{
                        optPatch
                          .orElse(obj.asInstanceOf[Option[a]])
                          .asInstanceOf[A]
                      }
                    else '{ optPatch.asInstanceOf[A] }
                  }
            }
          case _ =>
            val underlying = patchField[A, b, Config](path, field)
            '{
              new Patcher[A, B]:
                override def patch(obj: A, patch: B): A =
                  patch
                    .asInstanceOf[Option[b]]
                    .map(b => $underlying.patch(obj, b))
                    .getOrElse(obj)
            }
      case '[A] =>
        '{
          new Patcher[A, B]:
            override def patch(obj: A, patch: B): A = patch.asInstanceOf[A]
        }
      case _ =>
        Expr.summon[Patcher[A, B]] getOrElse derive[A, B, Config](Some(s"$path.$field"))
  end patchField

  private def hasPatcherConfig[Config <: Tuple: Type, Cfg <: PatcherCfg: Type]
    : Boolean =
    Type.of[Config] match
      case '[Cfg *: _]    => true
      case '[_ *: config] => hasPatcherConfig[config, Cfg]
      case _              => false
  end hasPatcherConfig

end ProductDerivePatcherMacros
