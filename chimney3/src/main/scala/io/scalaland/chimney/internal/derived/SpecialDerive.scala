package io.scalaland.chimney.internal.derived

import io.scalaland.chimney._
import io.scalaland.chimney.dsl._
import io.scalaland.chimney.internal.utils.MacroUtils
import io.scalaland.chimney.internal._
import io.scalaland.chimney.TransformerFSupport.support

import scala.quoted._
import scala.compiletime._
import scala.collection.Factory
import scala.collection.Map
import scala.collection.mutable.Builder
import scala.reflect.ClassTag

object SpecialDerive:
  import DeriveUtils._

  def deriveSpecialCases[From: Type, To: Type, Flags <: Tuple: Type, Path <: String: Type](using Quotes): Option[Expr[Transformer[From, To]]] =
    Type.of[From] match
      case '[Option[a]] =>
        Type.of[To] match
          case '[Option[b]] =>
            Some('{${deriveSpecialK1Impl[Option[a], Option[b], a, b, Flags, Path Concat ".?"](instance => '{opt => 
              opt match
                case Some(v) => Some(${instance}.transform(v))
                case None => None
            })}.asInstanceOf[Transformer[From, To]]})
          case _ =>
            None
      case '[Either[l, r]] =>
        Type.of[To] match
          case '[Either[tl, tr]] =>
            Some(deriveSpecialK2Impl[Either[l, r], Either[tl, tr], l, r, tl, tr, Flags, Path Concat ".\\/"](
              (lInstance, rInstance) => '{either =>
                either.fold(l => Left(${lInstance}.transform(l)), r => Right(${rInstance}.transform(r)))
            }).asInstanceOf[Expr[Transformer[From, To]]])
          case _ =>
            None
      case '[Array[(k, v)]] =>
        Type.of[To] match
          case '[IterableOnce[(tk, tv)]] =>
            deriveIterableLikeK2[From, To, k, v, tk, tv, Flags, Path Concat ".[*]"]('{_.asInstanceOf[Array[(k, v)]].iterator})
          case '[Array[(tk, tv)]] =>
            deriveIterableLikeK2[From, To, k, v, tk, tv, Flags, Path Concat ".[*]"]('{_.asInstanceOf[Array[(k, v)]].iterator})
          case _ =>
            None
      case '[Array[a]] =>
        Type.of[To] match
          case '[IterableOnce[b]] =>
            deriveIterableLike[From, To, a, b, Flags, Path Concat ".[*]"]('{_.asInstanceOf[Array[a]].iterator})
          case '[Array[b]] =>
            deriveIterableLike[From, To, a, b, Flags, Path Concat ".[*]"]('{_.asInstanceOf[Array[a]].iterator})
          case _ =>
            None
      case '[IArray[(k, v)]] =>
        Type.of[To] match
          case '[IterableOnce[(tk, tv)]] =>
            deriveIterableLikeK2[From, To, k, v, tk, tv, Flags, Path Concat ".[*]"]('{_.asInstanceOf[Array[(k, v)]].iterator})
          case '[Array[(tk, tv)]] =>
            deriveIterableLikeK2[From, To, k, v, tk, tv, Flags, Path Concat ".[*]"]('{_.asInstanceOf[Array[(k, v)]].iterator})
          case _ =>
            None
      case '[IArray[a]] =>
        Type.of[To] match
          case '[IterableOnce[b]] =>
            deriveIterableLike[From, To, a, b, Flags, Path Concat ".[*]"]('{_.asInstanceOf[IArray[a]].iterator})
          case '[Array[b]] =>
            deriveIterableLike[From, To, a, b, Flags, Path Concat ".[*]"]('{_.asInstanceOf[IArray[a]].iterator})
          case _ =>
            None
      case '[IterableOnce[(k, v)]] =>
        Type.of[To] match
          case '[IterableOnce[(tk, tv)]] =>
            deriveIterableLikeK2[From, To, k, v, tk, tv, Flags, Path Concat ".[*]"]('{_.asInstanceOf[IterableOnce[(k, v)]].iterator})
          case '[Array[(tk, tv)]] =>
            deriveIterableLikeK2[From, To, k, v, tk, tv, Flags, Path Concat ".[*]"]('{_.asInstanceOf[IterableOnce[(k, v)]].iterator})
          case _ =>
            None
      case '[IterableOnce[a]] =>
        Type.of[To] match
          case '[IterableOnce[b]] =>
            deriveIterableLike[From, To, a, b, Flags, Path Concat ".[*]"]('{_.asInstanceOf[IterableOnce[a]].iterator})
          case '[Array[b]] =>
            deriveIterableLike[From, To, a, b, Flags, Path Concat ".[*]"]('{_.asInstanceOf[IterableOnce[a]].iterator})
          case _ =>
            None
      case _ =>
        Type.of[To] match
          case '[Option[to]] =>
            Some(implementTransformerWith[From, To]('{from => Option(from).asInstanceOf[To]}))
          case _ =>
            None
  end deriveSpecialCases

  def deriveSpecialCasesF[F[_]: Type, From: Type, To: Type, Flags <: Tuple: Type, Path <: String: Type](sup: Expr[TransformerFSupport[F]])(using Quotes): Option[Expr[Any]] =
    Type.of[From] match
      case '[Option[a]] =>
        Type.of[To] match
          case '[Option[b]] =>
            Some(deriveSpecialK1FImpl[F, Option[a], Option[b], a, b, Flags, Path Concat ".?"](instance => '{opt => 
              opt match
                case Some(v) => $sup.map(${instance}.transform(v), Some(_))
                case None => $sup.pure(None)
            }, sup))
          case _ =>
            None
      case '[Either[l, r]] =>
        Type.of[To] match
          case '[F[Either[tl, tr]]] =>
            Some(deriveSpecialK2FImpl[F, Either[l, r], Either[tl, tr], l, r, tl, tr, Flags, Path Concat ".\\/"](
              (lInstance, rInstance) => '{either =>
                either match
                  case Left(l) => $sup.map($lInstance.transform(l), Left(_))
                  case Right(r) => $sup.map($rInstance.transform(r), Right(_))
            }, sup))
          case _ =>
            None
      case '[Array[(k, v)]] =>
        Type.of[To] match
          case '[IterableOnce[(tk, tv)]] =>
            deriveIterableLikeK2F[F, From, To, k, v, tk, tv, Flags, Path Concat ".[*]"]('{_.asInstanceOf[Array[(k, v)]].iterator}, sup)
          case '[Array[(tk, tv)]] =>
            deriveIterableLikeK2F[F, From, To, k, v, tk, tv, Flags, Path Concat ".[*]"]('{_.asInstanceOf[Array[(k, v)]].iterator}, sup)
          case _ =>
            None
      case '[Array[a]] =>
        Type.of[To] match
          case '[IterableOnce[b]] =>
            deriveIterableLikeF[F, From, To, a, b, Flags, Path Concat ".[*]"]('{_.asInstanceOf[Array[a]].iterator}, sup)
          case '[Array[b]] =>
            deriveIterableLikeF[F, From, To, a, b, Flags, Path Concat ".[*]"]('{_.asInstanceOf[Array[a]].iterator}, sup)
          case _ =>
            None
      case '[IArray[(k, v)]] =>
        Type.of[To] match
          case '[IterableOnce[(tk, tv)]] =>
            deriveIterableLikeK2F[F, From, To, k, v, tk, tv, Flags, Path Concat ".[*]"]('{_.asInstanceOf[Array[(k, v)]].iterator}, sup)
          case '[Array[(tk, tv)]] =>
            deriveIterableLikeK2F[F, From, To, k, v, tk, tv, Flags, Path Concat ".[*]"]('{_.asInstanceOf[Array[(k, v)]].iterator}, sup)
          case _ =>
            None
      case '[IArray[a]] =>
        Type.of[To] match
          case '[IterableOnce[b]] =>
            deriveIterableLikeF[F, From, To, a, b, Flags, Path Concat ".[*]"]('{_.asInstanceOf[IArray[a]].iterator}, sup)
          case '[Array[b]] =>
            deriveIterableLikeF[F, From, To, a, b, Flags, Path Concat ".[*]"]('{_.asInstanceOf[IArray[a]].iterator}, sup)
          case _ =>
            None
      case '[IterableOnce[(k, v)]] =>
        Type.of[To] match
          case '[IterableOnce[(tk, tv)]] =>
            deriveIterableLikeK2F[F, From, To, k, v, tk, tv, Flags, Path Concat ".[*]"]('{_.asInstanceOf[IterableOnce[(k, v)]].iterator}, sup)
          case '[Array[(tk, tv)]] =>
            deriveIterableLikeK2F[F, From, To, k, v, tk, tv, Flags, Path Concat ".[*]"]('{_.asInstanceOf[IterableOnce[(k, v)]].iterator}, sup)
          case _ =>
            None
      case '[IterableOnce[a]] =>
        Type.of[To] match
          case '[IterableOnce[b]] =>
            deriveIterableLikeF[F, From, To, a, b, Flags, Path Concat ".[*]"]('{_.asInstanceOf[IterableOnce[a]].iterator}, sup)
          case '[Array[b]] =>
            deriveIterableLikeF[F, From, To, a, b, Flags, Path Concat ".[*]"]('{_.asInstanceOf[IterableOnce[a]].iterator}, sup)
          case _ =>
            None
      case _ =>
        Type.of[To] match
          case '[Option[to]] =>
            Some(deriveSpecialLiftF[F, Option, From, To, to, Flags, Path](sup, '{support[Option]}))
          case '[Either[e, to]] =>
            Some(deriveSpecialLiftF[F, [t] =>> Either[e, t], From, To, to, Flags, Path](sup, '{support[[t] =>> Either[e, t]]}))
          case _ =>
            None
  end deriveSpecialCasesF

  private def deriveIterableLike[From: Type, To: Type, A: Type, B: Type, Flags <: Tuple: Type, Path <: String: Type](iteratorFrom: Expr[From => Iterator[A]])(using Quotes): Option[Expr[Transformer[From, To]]] =
    Expr.summon[Factory[B, To]] match
      case Some(factory) =>
        Some(deriveSpecialK1Impl[From, To, A, B, Flags, Path](
          instance => '{ from => 
            val b = $factory.newBuilder
            for elem <- $iteratorFrom(from) do b += $instance.transform(elem)
            b.result
          }
        ))
      case None =>
        None
  end deriveIterableLike

  private def deriveSpecialLiftF[F[_]: Type, L[_]: Type, From: Type, To: Type, B: Type, Flags <: Tuple: Type, Path <: String: Type](fSup: Expr[TransformerFSupport[F]], lSup: Expr[TransformerFSupport[L]])(using Quotes): Expr[TransformerF[F, From, To]] =
    val elemLifted: Option[Expr[TransformerF[F, From, B]]] = 
      Expr.summon[Transformer[From, B]].map(t => implementTransformerFWith[F, From, B]('{from => $fSup.pure($t.transform(from))}))
    val elemInstance: Expr[TransformerF[F, From, B]] = 
      Expr.summon[TransformerF[F, From, B]] orElse elemLifted getOrElse '{TransformerDerive.deriveConfiguredF[F, From, B, Path](configOfAtPath[B, Flags, Path](defaultDefinitionWithFlags))(using $fSup)}

    implementTransformerFWith[F, From, To]('{from => $lSup.traverseF($lSup.pure(from), $elemInstance.transform(_))(using $fSup).asInstanceOf[F[To]]})
  end deriveSpecialLiftF

  private def deriveIterableLikeF[F[_]: Type, From: Type, To: Type, A: Type, B: Type, Flags <: Tuple: Type, Path <: String: Type](iteratorFrom: Expr[From => Iterator[A]], sup: Expr[TransformerFSupport[F]])(using Quotes): Option[Expr[TransformerF[F, From, To]]] =
    Expr.summon[Factory[B, To]] match
      case Some(factory) =>
        Some(deriveSpecialK1FImpl[F, From, To, A, B, Flags, Path](
          instance => '{ from => $sup.traverse($iteratorFrom(from), $instance.transform(_))(using $factory)},
          sup
        ))
      case None =>
        None
  end deriveIterableLikeF

  private def deriveIterableLikeK2[From: Type, To: Type, A1: Type, A2: Type, B1: Type, B2: Type, Flags <: Tuple: Type, Path <: String: Type](iteratorFrom: Expr[From => Iterator[(A1, A2)]])(using Quotes): Option[Expr[Transformer[From, To]]] =
    Expr.summon[Factory[(B1, B2), To]] match
      case Some(factory) =>
        Some(deriveSpecialK2Impl[From, To, A1, A2, B1, B2, Flags, Path](
          (instance1, instance2) => '{ from => 
            val b = $factory.newBuilder
            for (_1, _2) <- $iteratorFrom(from) do b += ($instance1.transform(_1) -> $instance2.transform(_2))
            b.result
          }
        ))
      case None =>
        None
  end deriveIterableLikeK2

  private def deriveIterableLikeK2F[F[_]: Type, From: Type, To: Type, A1: Type, A2: Type, B1: Type, B2: Type, Flags <: Tuple: Type, Path <: String: Type](
    iteratorFrom: Expr[From => Iterator[(A1, A2)]], sup: Expr[TransformerFSupport[F]])(using Quotes): Option[Expr[TransformerF[F, From, To]]] =
    Expr.summon[Factory[(B1, B2), To]] match
      case Some(factory) =>
        Some(deriveSpecialK2FImpl[F, From, To, A1, A2, B1, B2, Flags, Path](
          (instance1, instance2) => '{ from => 
            $sup.traverse($iteratorFrom(from), (_1, _2) => $sup.product($instance1.transform(_1), $instance2.transform(_2)))(using $factory)
          },
          sup
        ))
      case None =>
        None
  end deriveIterableLikeK2F

  private def deriveSpecialK1Impl[From: Type, To: Type, EF: Type, ET: Type, Flags <: Tuple: Type, Path <: String: Type](compute: Expr[Transformer[EF, ET]] => Expr[From => To])(using Quotes): Expr[Transformer[From, To]] =
    val elemTransformer = Expr.summon[Transformer[EF, ET]] match
      case Some(t) => t
      case None => '{TransformerDerive.deriveConfigured[EF, ET, Path](configOfAtPath[ET, Flags, Path](defaultDefinitionWithFlags))}

    implementTransformerWith(compute(elemTransformer))
  end deriveSpecialK1Impl

  private def deriveSpecialK1FImpl[F[_]: Type, From: Type, To: Type, EF: Type, ET: Type, Flags <: Tuple: Type, Path <: String: Type](compute: Expr[TransformerF[F, EF, ET]] => Expr[From => F[To]], sup: Expr[TransformerFSupport[F]])(using Quotes): Expr[TransformerF[F, From, To]] =

    val elemPureTransformer = Expr.summon[Transformer[EF, ET]].map(t => implementTransformerFWith[F, EF, ET]('{from => $sup.pure($t.transform(from))}))
    val elemTransformer = Expr.summon[TransformerF[F, EF, ET]] orElse elemPureTransformer getOrElse '{TransformerDerive.deriveConfiguredF[F, EF, ET, Path](configOfAtPath[ET, Flags, Path](defaultDefinitionWithFlags))(using $sup)}

    implementTransformerFWith(compute(elemTransformer))
  end deriveSpecialK1FImpl

  private def deriveSpecialK2Impl[From: Type, To: Type, F1: Type, F2: Type, T1: Type, T2: Type, Flags <: Tuple: Type, Path <: String: Type](
    compute: (Expr[Transformer[F1, T1]], Expr[Transformer[F2, T2]]) => Expr[From => To])(using Quotes): Expr[Transformer[From, To]] =
    val elemTransformer1 = Expr.summon[Transformer[F1, T1]] match
      case Some(t) => t
      case None => '{TransformerDerive.deriveConfigured[F1, T1, Path](configOfAtPath[T1, Flags, Path](defaultDefinitionWithFlags))}
    
    val elemTransformer2 = Expr.summon[Transformer[F2, T2]] match
      case Some(t) => t
      case None => '{TransformerDerive.deriveConfigured[F2, T2, Path](configOfAtPath[T2, Flags, Path](defaultDefinitionWithFlags))}

    implementTransformerWith(compute(elemTransformer1, elemTransformer2))
  end deriveSpecialK2Impl

  private def deriveSpecialK2FImpl[F[_]: Type, From: Type, To: Type, F1: Type, F2: Type, T1: Type, T2: Type, Flags <: Tuple: Type, Path <: String: Type](
    compute: (Expr[TransformerF[F, F1, T1]], Expr[TransformerF[F, F2, T2]]) => Expr[From => F[To]],
    sup: Expr[TransformerFSupport[F]]
  )(using Quotes): Expr[TransformerF[F, From, To]] =
    val elem1PureTransformer = Expr.summon[Transformer[F1, T1]].map(t => implementTransformerFWith[F, F1, T1]('{from => $sup.pure($t.transform(from))}))
    val elemTransformer1 = Expr.summon[TransformerF[F, F1, T1]] orElse elem1PureTransformer getOrElse '{TransformerDerive.deriveConfiguredF[F, F1, T1, Path](configOfAtPath[T1, Flags, Path](defaultDefinitionWithFlags))(using $sup)}
    
    val elem2PureTransformer = Expr.summon[Transformer[F2, T2]].map(t => implementTransformerFWith[F, F2, T2]('{from => $sup.pure($t.transform(from))}))
    val elemTransformer2 = Expr.summon[TransformerF[F, F2, T2]] orElse elem2PureTransformer getOrElse '{TransformerDerive.deriveConfiguredF[F, F2, T2, Path](configOfAtPath[T2, Flags, Path](defaultDefinitionWithFlags))(using $sup)}

    implementTransformerFWith(compute(elemTransformer1, elemTransformer2))
  end deriveSpecialK2FImpl

end SpecialDerive