package io.scalaland.chimney.internal.derived

import io.scalaland.chimney.Patcher
import io.scalaland.chimney.internal.PatcherCfg
import io.scalaland.chimney.internal.utils.modules.*

import scala.collection.{Factory, Iterable}
import scala.quoted.*

trait IterablePatcherDeriveMacros:
  self: PatcherDeriveMacros & FieldModule & MirrorModule & Module =>
  import quotes.reflect.*

  private val optimizationLimit: Int = 3

  def deriveMap[A: Type, B: Type, Config <: Tuple: Type](
    path: String
  ): Option[Expr[Patcher[A, B]]] =
    Type.of[A] match
      case '[Map[ak, av]] =>
        Type.of[B] match
          case '[Map[ak, bv]] =>
            Expr.summon[Factory[(ak, av), A]] match
              case Some(factory) =>
                val elemPatcher =
                  Expr.summon[Patcher[av, bv]] getOrElse derive[av, bv, Config](
                    Some(s"$path.{*}")
                  )
                Some('{
                  new Patcher[A, B]:
                    override def patch(obj: A, patch: B): A =
                      val mapObj = obj.asInstanceOf[Map[ak, av]]
                      val mapPatch = patch.asInstanceOf[Map[ak, bv]]
                      if mapPatch.size <= ${ Expr(optimizationLimit) } then
                        ${
                          processMapOpt[A, ak, av, bv, Config](
                            '{ mapObj },
                            '{ mapPatch },
                            elemPatcher
                          )
                        }
                      else
                        ${
                          processMap[A, ak, av, bv, Config](
                            '{ mapObj },
                            '{ mapPatch },
                            factory,
                            elemPatcher
                          )
                        }
                    end patch
                })
              case _ =>
                report.errorAndAbort(
                  s"Unable to derive collection instance, probably a library bug, encountered at: $path"
                )
      case _ =>
        None
  end deriveMap

  def deriveIterable[A: Type, B: Type, Config <: Tuple: Type](
    path: String
  ): Option[Expr[Patcher[A, B]]] =
    (Type.of[A], Type.of[B]) match
      case ('[Iterable[a]], '[Iterable[b]]) =>
        Expr.summon[Factory[a, A]] match
          case Some(factory) =>
            val elemPatcher =
              Expr.summon[Patcher[a, b]] getOrElse derive[a, b, Config](
                Some(s"$path.[*]")
              )

            Some('{
              new Patcher[A, B]:
                override def patch(obj: A, patch: B): A =
                  val objIter = obj.asInstanceOf[Iterable[a]]
                  val patchIter = patch.asInstanceOf[Iterable[b]]
                  val builder = $factory.newBuilder
                  ${
                    if canOverride[Config, a, b] then
                      '{
                        for p <- patchIter do builder += p.asInstanceOf[a]
                        builder.result()
                      }
                    else
                      '{
                        objIter.iterator
                          .map(Some(_))
                          .zipAll(patchIter.iterator.map(Some(_)), None, None)
                          .collect { case (Some(obj), patch) => obj -> patch }
                          .foreach { (obj, patch) =>
                            patch match
                              case Some(patch) =>
                                builder += $elemPatcher.patch(obj, patch)
                              case None => builder += obj
                          }
                        builder.result()
                      }
                    end if
                  }
                end patch
            })
          case None =>
            report.errorAndAbort(
              s"Unable to derive collection instance, probably a library bug, encountered at: $path"
            )
      case _ =>
        None
  end deriveIterable

  private def processMap[
    A: Type,
    K: Type,
    AV: Type,
    BV: Type,
    Config <: Tuple: Type
  ](
    source: Expr[Map[K, AV]],
    patch: Expr[Map[K, BV]],
    factory: Expr[Factory[(K, AV), A]],
    elemPatcher: Expr[Patcher[AV, BV]]
  ): Expr[A] =
    '{
      val builder = $factory.newBuilder
      $source.foreach { (k, v) =>
        $patch.get(k) match
          case Some(pv) =>
            builder += (k -> $elemPatcher.patch(v, pv))
          case None =>
            ${
              if hasPatcherConfig[Config, PatcherCfg.IgnoreNoneInPatch] then
                '{ builder += (k -> v) }
              else '{}
            }
      }
      ${
        if canOverride[Config, AV, BV] then
          '{ builder ++= $patch.asInstanceOf[Map[K, AV]] }
        else '{}
        end if
      }
      builder.result()
    }
  end processMap

  private def processMapOpt[
    A: Type,
    K: Type,
    AV: Type,
    BV: Type,
    Config <: Tuple: Type
  ](
    source: Expr[Map[K, AV]],
    patch: Expr[Map[K, BV]],
    elemPatcher: Expr[Patcher[AV, BV]]
  ): Expr[A] =
    '{
      val initialMap = ${
        if hasPatcherConfig[Config, PatcherCfg.IgnoreNoneInPatch] then source
        else '{ Map.empty[K, AV] }
        end if
      }
      val finalMap = $patch.foldLeft(initialMap) { case (newMap, (pk, pv)) =>
        newMap.get(pk) match
          case Some(sv) =>
            newMap.updated(pk, $elemPatcher.patch(sv, pv))
          case None =>
            ${
              if canOverride[Config, AV, BV] then
                '{ newMap.updated(pk, pv.asInstanceOf[AV]) }
              else '{ newMap }
            }
      }
      finalMap.asInstanceOf[A]
    }
  end processMapOpt

  private def canOverride[Config <: Tuple: Type, A: Type, B: Type]: Boolean =
    Type.of[B] match
      case '[A] =>
        hasPatcherConfig[Config, PatcherCfg.OverwriteIterablesOnTheSameType]
  end canOverride

  private def hasPatcherConfig[Config <: Tuple: Type, Cfg <: PatcherCfg: Type]
    : Boolean =
    Type.of[Config] match
      case '[Cfg *: _]    => true
      case '[_ *: config] => hasPatcherConfig[config, Cfg]
      case _              => false
  end hasPatcherConfig

end IterablePatcherDeriveMacros
