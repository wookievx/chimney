package io.scalaland.chimney.internal.derived

import io.scalaland.chimney._
import io.scalaland.chimney.dsl._
import io.scalaland.chimney.internal.utils.MacroUtils
import io.scalaland.chimney.internal._
import io.scalaland.chimney.TransformerFSupport.support

import scala.compiletime._
import scala.collection.Factory
import scala.collection.Map
import scala.collection.mutable.Builder
import scala.reflect.ClassTag

object SpecialDerive:
  import DeriveUtils._

  private given identity[T]: Transformer[T, T] with
    def transform(from: T): T = from

  //unlawfull instance to allow unifying option handling with collections, hack, but want to make it work
  private def optionInstance[T] = new Factory[T, Option[T]]:
    def fromSpecific(it: IterableOnce[T]): Option[T] = {
      val iter = it.iterator
      iter.nextOption()
    }
    def newBuilder: Builder[T, Option[T]] = new Builder[T, Option[T]]:
      private var res: Option[T] = None
      override def clear(): Unit = res = None
      override def result(): Option[T] = res
      override def addOne(elem: T): this.type =
        res = Some(elem)
        this

  transparent inline def deriveSpecialCases[From, To, Flags <: Tuple, Path <: String]: Option[Transformer[From, To]] =
    inline erasedValue[From] match
      case _: Option[a] =>
        inline erasedValue[To] match
          case _: Option[b] =>
            Some(deriveOpt[a, b, Flags, Path].asInstanceOf[Transformer[From, To]])
          case _ =>
            None
      case _: Either[l, r] =>
        inline erasedValue[To] match
          case _: Either[tl, tr] =>
            Some(deriveEither[l, r, tl, tr, Flags, Path].asInstanceOf[Transformer[From, To]])
          case _ =>
            None
      case _: Array[a] =>
        inline erasedValue[To] match
          case _: Array[b] =>
            given ctb: ClassTag[b] = summonInline[ClassTag[b]]
            Some(deriveArray[a, b, Flags, Path].asInstanceOf[Transformer[From, To]])
          case _: IterableOnce[b] =>
            summonFrom {
              case factory: Factory[b, To] =>
                Some(iterableLikeDerive[Array[a], a, IterableOnce[b], b, Flags, Path](_.iterator)(using factory.asInstanceOf).asInstanceOf[Transformer[From, To]])
              case _ =>
                None
            }
          case _ =>
            None
      case _: IArray[a] =>
        inline erasedValue[To] match
          case _: IArray[b] =>
            given ctb: ClassTag[b] = summonInline[ClassTag[b]]
            Some(deriveIArray[a, b, Flags, Path].asInstanceOf[Transformer[From, To]])
          case _: IterableOnce[b] =>
            summonFrom {
              case factory: Factory[b, To] =>
                Some(iterableLikeDerive[IArray[a], a, IterableOnce[b], b, Flags, Path](_.iterator)(using factory.asInstanceOf).asInstanceOf[Transformer[From, To]])
              case _ =>
                None
            }
          case _ =>
            None
      case _: Map[ka, a] =>
        inline erasedValue[To] match
          case _: Map[kb, b] =>
            summonFrom {
              case factory: Factory[(kb, b), To] =>
                Some(deriveMap[ka, kb, a, b, Flags, Path](using factory.asInstanceOf).asInstanceOf[Transformer[From, To]])
              case _ =>
                MacroUtils.reportErrorAtPath(constValue[Path], "Unable to derive map instance, probably a library bug, encountered at: ")
            }
      case _: IterableOnce[a] =>
        inline erasedValue[To] match
          case _: IterableOnce[b] =>
            summonFrom {
              case factory: Factory[b, To] =>
                Some(iterableLikeDerive[From, a, IterableOnce[b], b, Flags, Path](_.asInstanceOf[IterableOnce[a]].iterator)(using factory.asInstanceOf).asInstanceOf[Transformer[From, To]])
              case _ =>
                MacroUtils.reportErrorAtPath(constValue[Path], "Unable to derive collection instance, probably a library bug, encountered at: ")
            }
          case _: Array[b] =>
            given classtag: ClassTag[b] = summonInline[ClassTag[b]]
            Some(iterableLikeDerive[From, a, Array[b], b, Flags, Path](_.asInstanceOf[IterableOnce[a]].iterator).asInstanceOf[Transformer[From, To]])
          case _: IArray[b] =>
            given classtag: ClassTag[b] = summonInline[ClassTag[b]]
            //a bit nasty, but given definition of IArray will work
            Some(iterableLikeDerive[From, a, Array[b], b, Flags, Path](_.asInstanceOf[IterableOnce[a]].iterator).asInstanceOf[Transformer[From, To]])
          case _ =>
            None
      case _ => None
      
  transparent inline def deriveSpecialCasesF[F[_], From, To, Flags <: Tuple, Path <: String](using TransformerFSupport[F]) =
    inline erasedValue[From] match
      case _: Option[a] =>
        inline erasedValue[To] match
          case _: Option[b] =>
            Some(deriveOpt[a, b, Flags, Path])
          case _: F[Option[b]] =>
            Some(deriveOptF[F, a, b, Flags, Path])
          case _ =>
            None
      case _: Either[l, r] =>
        inline erasedValue[To] match
          case _: Either[tl, tr] =>
            Some(deriveEither[l, r, tl, tr, Flags, Path])
          case _: F[Either[tl, tr]] =>
            Some(deriveEitherF[F, l, r, tl, tr, Flags, Path])
          case _ =>
            None
      case _: Array[a] =>
        inline erasedValue[To] match
          case _: Array[b] =>
            given ctb: ClassTag[b] = summonInline[ClassTag[b]]
            Some(deriveArray[a, b, Flags, Path])
          case _: F[Array[b]] =>
            given ctb: ClassTag[b] = summonInline[ClassTag[b]]
            Some(deriveArrayF[F, a, b, Flags, Path])
          case _: IterableOnce[b] =>
            summonFrom {
              case factory: Factory[b, To] =>
                Some(iterableLikeDerive[Array[a], a, IterableOnce[b], b, Flags, Path](_.iterator)(using factory.asInstanceOf).asInstanceOf[Transformer[From, To]])
              case _ =>
                None
            }
          case _: F[IterableOnce[b]] =>
            summonFrom {
              case factory: Factory[b, To] =>
                Some(iterableLikeDeriveF[F, Array[a], a, IterableOnce[b], b, Flags, Path](_.iterator)(using factory.asInstanceOf).asInstanceOf[TransformerF[F, From, To]])
              case _ =>
                None
            }            
          case _ =>
            None
      case _: IArray[a] =>
        inline erasedValue[To] match
          case _: IArray[b] =>
            given ctb: ClassTag[b] = summonInline[ClassTag[b]]
            Some(deriveIArray[a, b, Flags, Path])
          case _: F[Array[b]] =>
            given ctb: ClassTag[b] = summonInline[ClassTag[b]]
            Some(deriveIArrayF[F, a, b, Flags, Path])
          case _: IterableOnce[b] =>
            summonFrom {
              case factory: Factory[b, To] =>
                Some(iterableLikeDerive[IArray[a], a, IterableOnce[b], b, Flags, Path](_.iterator)(using factory.asInstanceOf).asInstanceOf[Transformer[From, To]])
              case _ =>
                None
            }
          case _: F[IterableOnce[b]] =>
            summonFrom {
              case factory: Factory[b, To] =>
                Some(iterableLikeDeriveF[F, Array[a], a, IterableOnce[b], b, Flags, Path](_.iterator)(using factory.asInstanceOf).asInstanceOf[TransformerF[F, From, To]])
              case _ =>
                None
            }
          case _ =>
            None      
      case _: IterableOnce[a] =>
        inline erasedValue[To] match
          case _: IterableOnce[b] =>
            summonFrom {
              case factory: Factory[b, To] =>
                Some(iterableLikeDeriveF[F, From, a, IterableOnce[b], b, Flags, Path](_.asInstanceOf[IterableOnce[a]].iterator)(using factory.asInstanceOf))
              case _ =>
                MacroUtils.reportErrorAtPath(constValue[Path], "Unable to derive collection instance, probably a library bug, encountered at: ")
            }
          case _: F[x] =>
            inline erasedValue[x] match
              case _: IterableOnce[b] =>
                summonFrom {
                  case factory: Factory[b, x] =>
                    Some(iterableLikeDeriveF[F, From, a, x, b, Flags, Path](_.asInstanceOf[IterableOnce[a]].iterator)(using factory))
                  case _ =>
                    MacroUtils.reportErrorAtPath(constValue[Path], "Unable to derive collection instance, probably a library bug, encountered at: ")
                }
              case _: Array[b] =>
                given classtag: ClassTag[b] = summonInline[ClassTag[b]]
                Some(iterableLikeDeriveF[F, From, a, Array[b], b, Flags, Path](_.asInstanceOf[IterableOnce[a]].iterator))
              case _: IArray[b] =>
                given classtag: ClassTag[b] = summonInline[ClassTag[b]]
                //a bit nasty, but given definition of IArray will work
                Some(iterableLikeDeriveF[F, From, a, Array[b], b, Flags, Path](_.asInstanceOf[IterableOnce[a]].iterator).asInstanceOf[TransformerF[F, From, To]])
              case _ =>
                None
          case _: Array[b] =>
            given classtag: ClassTag[b] = summonInline[ClassTag[b]]
            Some(iterableLikeDerive[From, a, Array[b], b, Flags, Path](_.asInstanceOf[IterableOnce[a]].iterator).asInstanceOf[Transformer[From, To]])
          case _: IArray[b] =>
            given classtag: ClassTag[b] = summonInline[ClassTag[b]]
            //a bit nasty, but given definition of IArray will work
            Some(iterableLikeDerive[From, a, Array[b], b, Flags, Path](_.asInstanceOf[IterableOnce[a]].iterator).asInstanceOf[Transformer[From, To]])
          case _ =>
            None
      case _: F[x] =>
        inline erasedValue[To] match
          case _: F[y] =>
            Some(deriveSupport[F, x, y, Flags, Path])
          case _ =>
            None
      case _ =>
        inline erasedValue[To] match
          case _: F[y] =>
            Some(TransformerDerive.deriveConfiguredF[F, From, y, Concat[Path, "F[*]"]](configOfAtPath[y, Flags, Concat[Path, "F[*]"]](defaultDefinitionWithFlags)))
          case _ =>
            None

  inline def deriveMap[KA, KB, A, B, Flags <: Tuple, Path <: String](using Factory[(KB, B), Map[KB, B]]): Transformer[Map[KA, A], Map[KB, B]] =
    val keyTansform = summonFrom {
      case inst: Transformer[KA, KB] => inst
      case _ =>
        TransformerDerive.deriveConfigured[KA, KB, Concat[Path, "{*:}"]](configOfAtPath[KB, Flags, Concat[Path, "{*:}"]](defaultDefinitionWithFlags))
    }
    val valueTransform = summonFrom {
      case inst: Transformer[A, B] => inst
      case _ =>
        TransformerDerive.deriveConfigured[A, B, Concat[Path, "{:*}"]](configOfAtPath[B, Flags, Concat[Path, "{:*}"]](defaultDefinitionWithFlags))
    }
    transformerWith[Map[KA, A], Map[KB, B]] { from =>
      val builder = summon[Factory[(KB, B), Map[KB, B]]].newBuilder
      for (k, v) <- from.iterator do builder.addOne(keyTansform.transform(k) -> valueTransform.transform(v))
      builder.result
    }
  end deriveMap
  
  inline def deriveSupport[F[_], From, To, Flags <: Tuple, Path <: String](using TransformerFSupport[F]): Transformer[F[From], F[To]] =
    val underlyingTransform = summonFrom {
      case inst: Transformer[From, To] => inst
      case _ =>
        TransformerDerive.deriveConfigured[From, To, Concat[Path, "F[*]"]](configOfAtPath[To, Flags, Concat[Path, "F[*]"]](defaultDefinitionWithFlags))
    }
    transformerWith[F[From], F[To]](support[F].map(_, underlyingTransform.transform))

  inline def deriveOpt[A, B, Flags <: Tuple, Path <: String]: Transformer[Option[A], Option[B]] =
    val underlyingTransform = summonFrom {
      case inst: Transformer[A, B] => inst
      case _ =>
        TransformerDerive.deriveConfigured[A, B, Concat[Path, "?*"]](configOfAtPath[B, Flags, Concat[Path, "?*"]](defaultDefinitionWithFlags))
    }
    MacroUtils.printAtCompileTime["Wtf why not compiling at: " Concat Path]
    OptionTransformer {
      case Some(v) => Some(underlyingTransform.transform(v))
      case None => None
    }
  end deriveOpt

  inline def deriveOptF[F[_], A, B, Flags <: Tuple, Path <: String](using TransformerFSupport[F]): TransformerF[F, Option[A], Option[B]] =
    val underlyingTransform = summonFrom {
      case inst: TransformerF[F, A, B] => inst
      case _ =>
        TransformerDerive.deriveConfiguredF[F, A, B, Concat[Path, "?*"]](configOfAtPath[B, Flags, Concat[Path, "?*"]](defaultDefinitionWithFlags))
    }
    OptionTransformerF {
      case Some(v) => support[F].map(underlyingTransform.transform(v), Some(_))
      case None => support[F].pure(None)
    }
  end deriveOptF
  
  inline def deriveEither[L1, R1, L2, R2, Flags <: Tuple, Path <: String]: Transformer[Either[L1, R1], Either[L2, R2]] =
    val underlyingLeft = summonFrom {
      case inst: Transformer[L1, L2] => inst
      case _ =>
        TransformerDerive.deriveConfigured[L1, L2, Concat[Path, "[*\\/]"]](configOfAtPath[L2, Flags, Concat[Path, "[*\\/]"]](defaultDefinitionWithFlags))
    }
    val underlyingRight = summonFrom {
      case inst: Transformer[R1, R2] => inst
      case _ =>
        TransformerDerive.deriveConfigured[R1, R2, Concat[Path, "[\\/*]"]](configOfAtPath[L2, Flags, Concat[Path, "[\\/*]"]](defaultDefinitionWithFlags))
    }
    transformerWith[Either[L1, R1], Either[L2, R2]] { from =>
      from match
        case Left(l1) => Left(underlyingLeft.transform(l1))
        case Right(r1) => Right(underlyingRight.transform(r1))
    }
  end deriveEither

  inline def deriveEitherF[F[_], L1, R1, L2, R2, Flags <: Tuple, Path <: String](using TransformerFSupport[F]): TransformerF[F, Either[L1, R1], Either[L2, R2]] =
    val underlyingLeft = summonFrom {
      case inst: TransformerF[F, L1, L2] => inst
      case _ =>
        TransformerDerive.deriveConfiguredF[F, L1, L2, Concat[Path, "[*\\/]"]](configOfAtPath[L2, Flags, Concat[Path, "[*\\/]"]](defaultDefinitionWithFlags))
    }
    val underlyingRight = summonFrom {
      case inst: TransformerF[F, R1, R2] => inst
      case _ =>
        TransformerDerive.deriveConfiguredF[F, R1, R2, Concat[Path, "[\\/*]"]](configOfAtPath[L2, Flags, Concat[Path, "[\\/*]"]](defaultDefinitionWithFlags))
    }
    transformerWithF[F, Either[L1, R1], Either[L2, R2]] { from =>
      from match
        case Left(l1) => support.map(underlyingLeft.transform(l1), Left(_))
        case Right(r1) => support.map(underlyingRight.transform(r1), Right(_))
    }
  end deriveEitherF

  inline def deriveArray[A, B: ClassTag, Flags <: Tuple, Path <: String]: Transformer[Array[A], Array[B]] =
    iterableLikeDerive[Array[A], A, Array[B], B, Flags, Path](_.iterator)

  inline def deriveIArray[A, B: ClassTag, Flags <: Tuple, Path <: String]: Transformer[IArray[A], IArray[B]] = 
    deriveArray[A, B, Flags, Path].asInstanceOf

  inline def deriveArrayF[F[_], A, B: ClassTag, Flags <: Tuple, Path <: String](using TransformerFSupport[F]): TransformerF[F, Array[A], Array[B]] =
    iterableLikeDeriveF[F, Array[A], A, Array[B], B, Flags, Path](_.iterator)

  inline def deriveIArrayF[F[_], A, B: ClassTag, Flags <: Tuple, Path <: String](using TransformerFSupport[F]): TransformerF[F, IArray[A], IArray[B]] =
    deriveArrayF[F, A, B, Flags, Path].asInstanceOf

  private inline def iterableLikeDerive[From, Felem, To, Telem, Flags <: Tuple, Path <: String](inline getIterator: From => Iterator[Felem])(using Factory[Telem, To]): Transformer[From, To] =
    val elemTransform = summonFrom {
      case inst: Transformer[Felem, Telem] => inst
      case _ =>
        TransformerDerive.deriveConfigured[Felem, Telem, Concat[Path, "[*]"]](configOfAtPath[Telem, Flags, Concat[Path, "[*]"]](defaultDefinitionWithFlags))
    }

    transformerWith[From, To] { from =>
      val b = summon[Factory[Telem, To]].newBuilder
      for elem <- getIterator(from) do b += elemTransform.transform(elem)
      b.result
    }
  end iterableLikeDerive

  private inline def iterableLikeDeriveF[F[_], From, Felem, To, Telem, Flags <: Tuple, Path <: String](inline getIterator: From => Iterator[Felem])(using Factory[Telem, To], TransformerFSupport[F]): TransformerF[F, From, To] =
    val elemTransform = summonFrom {
      case inst: TransformerF[F, Felem, Telem] => inst
      case _ =>
        TransformerDerive.deriveConfiguredF[F, Felem, Telem, Concat[Path, "[*]"]](configOfAtPath[Telem, Flags, Concat[Path, "[*]"]](defaultDefinitionWithFlags))
    }

    transformerWithF[F, From, To](from => support[F].traverse(getIterator(from), elemTransform.transform))
  end iterableLikeDeriveF


  class OptionTransformer[A, B, Flags <: Tuple, Path <: String](impl: Option[A] => Option[B]) extends Transformer[Option[A], Option[B]]:
    def transform(from: Option[A]): Option[B] = impl(from)
  end OptionTransformer

  class OptionTransformerF[F[_], A, B, Flags <: Tuple, Path <: String](impl: Option[A] => F[Option[B]])(using TransformerFSupport[F]) extends TransformerF[F, Option[A], Option[B]]:
    def transform(from: Option[A]): F[Option[B]] = impl(from)
  end OptionTransformerF
end SpecialDerive