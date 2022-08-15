package io.scalaland.chimney.internal.derived

import io.scalaland.chimney.internal.utils.modules.{
  CommonDeriveModule,
  ConfigModule,
  FieldModule,
  MirrorModule,
  Module
}

import scala.quoted.*

trait CoproductDeriveMacros:
  self: TransformerDeriveMacros & FieldModule & MirrorModule & ConfigModule &
    Module =>
  import quotes.reflect.*

  def deriveCoproduct[A: Type, B: Type, Flags <: Tuple: Type](
    source: Expr[A],
    config: TransformerDefinitionMaterialized[Flags],
    sourceMirror: CoproductMirror[A],
    targetMirror: CoproductMirror[B]
  ): Expr[B] =
    val sourceCases = sourceMirror.cases
    val targetCases = targetMirror.cases.view.map((_, p) => p).toMap
    '{
      val ordinal = ${ sourceMirror.caseOrdinality }($source)
      ${
        handleBranches[A, B, Flags](
          source,
          '{ ordinal },
          config,
          sourceCases,
          targetCases
        )
      }
    }
  end deriveCoproduct

  private def handleBranches[A: Type, B: Type, Flags <: Tuple: Type](
    source: Expr[A],
    ordinal: Expr[Int],
    config: TransformerDefinitionMaterialized[Flags],
    sourceCases: List[(Int, (String, TypeRepr))],
    targetCases: Map[String, TypeRepr]
  ): Expr[B] =
    sourceCases match
      case (caseOrdinal, (caseName, caseTpe)) :: sourceCases =>
        val computedCaseLogic = config
          .isCaseComputed(caseName)
          .map(func =>
            caseTpe.asType match
              case '[a] =>
                '{
                  if $ordinal == ${ Expr(caseOrdinal) } then
                    $func.asInstanceOf[a => B]($source.asInstanceOf[a])
                  else
                    ${
                      handleBranches[A, B, Flags](
                        source,
                        ordinal,
                        config,
                        sourceCases,
                        targetCases
                      )
                    }
                  end if
                }
          )
        val defaultLogic = targetCases.get(caseName) match
          case Some(targetTpe) =>
            (caseTpe.asType, targetTpe.asType) match
              case ('[a], '[b]) =>
                Some(
                  '{
                    if $ordinal == ${ Expr(caseOrdinal) } then
                      ${
                        elemWiseTransform[a, b, Flags](
                          '{ $source.asInstanceOf[a] },
                          config
                        ).asInstanceOf[Expr[B]]
                      }
                    else
                      ${
                        handleBranches[A, B, Flags](
                          source,
                          ordinal,
                          config,
                          sourceCases,
                          targetCases
                        )
                      }
                    end if
                  }
                )
          case None =>
            None

        computedCaseLogic orElse defaultLogic getOrElse
          report.errorAndAbort(
            s"No case named $caseName in target type ${Type.show[B]}"
          )
      case Nil =>
        '{
          throw RuntimeException(
            "Unhandled condition encountered during Coproduct Transformer derivation"
          )
        }
  end handleBranches

end CoproductDeriveMacros
