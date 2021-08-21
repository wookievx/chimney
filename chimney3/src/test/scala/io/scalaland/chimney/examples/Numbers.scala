package io.scalaland.chimney.examples

import io.scalaland.chimney.{Transformer, TransformerF, TransformerFSupport}
import io.scalaland.chimney.internal.derived.DeriveUtils
import io.scalaland.chimney.internal.utils.MacroUtils
import scala.compiletime.summonFrom

  // following https://en.wikipedia.org/wiki/Names_of_large_numbers

object short {
  sealed trait NumScale[+T, Dummy]
  case object Zero extends NumScale[Nothing, Nothing]
  case class Million[T](count: T) extends NumScale[T, Nothing] // 10^6
  case class Billion[T](count: T) extends NumScale[T, Nothing] // 10^9
  case class Trillion[T](count: T) extends NumScale[T, Nothing] // 10^12
  case object Infinity extends NumScale[Nothing, Nothing]
}

object long {
  sealed trait NumScale[+T]
  case object Zero extends NumScale[Nothing]
  case class Million[T](count: T) extends NumScale[T] // 10^6
  case class Milliard[T](count: T) extends NumScale[T] // 10^9
  case class Billion[T](count: T) extends NumScale[T] // 10^12
  case class Billiard[T](count: T) extends NumScale[T] // 10^15
  case class Trillion[T](count: T) extends NumScale[T] // 10^18
  case object Infinity extends NumScale[Nothing] //for checking if custom instances are picked up
}

object ScalesTransformer {
  import io.scalaland.chimney.dsl.*

  inline given shortToLongGen[F[_], A, B](using sup: TransformerFSupport[F]): TransformerF[F, short.NumScale[A, Nothing], long.NumScale[B]] = 
    summonFrom {
      case ft: TransformerF[F, A, B] => shortToLongWrappedInner[F, A, B](using sup, ft)
      case ft: Transformer[A, B] => shortToLongPureInner[F, A, B](using sup, ft)
    }
  end shortToLongGen

  inline def shortToLongPureInner[F[_]: TransformerFSupport, A, B](
    using ft: Transformer[A, B]
  ): TransformerF[F, short.NumScale[A, Nothing], long.NumScale[B]] = {
    inline Transformer
      .defineF[F, short.NumScale[A, Nothing], long.NumScale[B]]
      .withCoproductInstance[short.Billion[A], long.Milliard[B]] { billion =>
        billion.transformInto[long.Milliard[B]]
      }
      .withCoproductInstance[short.Trillion[A], long.Billion[B]] { trillion =>
        trillion.transformInto[long.Billion[B]]
      }.withCoproductInstance[short.Infinity.type, long.Infinity.type] { _ =>
        long.Infinity
      } match
        case definition: TransformerFDefinition[F, short.NumScale[A, Nothing], long.NumScale[B], config, flags] =>
          definition.buildTransformer
  }

  inline def shortToLongWrappedInner[F[_]: TransformerFSupport, A, B](
      using ft: TransformerF[F, A, B]
  ): TransformerF[F, short.NumScale[A, Nothing], long.NumScale[B]] = {
    inline Transformer
      .defineF[F, short.NumScale[A, Nothing], long.NumScale[B]]
      .withCoproductInstanceF[short.Billion[A], long.Milliard[B]] { billion =>
        billion.transformIntoF[F, long.Milliard[B]]
      }
      .withCoproductInstanceF[short.Trillion[A], long.Billion[B]] { trillion =>
        trillion.transformIntoF[F, long.Billion[B]]
      }.withCoproductInstance[short.Infinity.type, long.Infinity.type] { _ =>
        long.Infinity
      } match
        case definition: TransformerFDefinition[F, short.NumScale[A, Nothing], long.NumScale[B], config, flags] =>
          definition.buildTransformer
  }

}
