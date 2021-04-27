package io.scalaland.chimney.examples

import io.scalaland.chimney.{Transformer, TransformerF, TransformerFSupport}
import io.scalaland.chimney.internal.derived.DeriveUtils
import io.scalaland.chimney.internal.utils.MacroUtils

  // following https://en.wikipedia.org/wiki/Names_of_large_numbers

object short {
  sealed trait NumScale[+T, Dummy]
  case object Zero extends NumScale[Nothing, Nothing]
  case class Million[T](count: T) extends NumScale[T, Nothing] // 10^6
  case class Billion[T](count: T) extends NumScale[T, Nothing] // 10^9
  case class Trillion[T](count: T) extends NumScale[T, Nothing] // 10^12
}

object long {
  sealed trait NumScale[+T]
  case object Zero extends NumScale[Nothing]
  case class Million[T](count: T) extends NumScale[T] // 10^6
  case class Milliard[T](count: T) extends NumScale[T] // 10^9
  case class Billion[T](count: T) extends NumScale[T] // 10^12
  case class Billiard[T](count: T) extends NumScale[T] // 10^15
  case class Trillion[T](count: T) extends NumScale[T] // 10^18
}

object ScalesTransformer {
  import io.scalaland.chimney.dsl._

  inline given shortToLongPureInner[F[_]: TransformerFSupport, A, B](
    using ft: Transformer[A, B]
  ): TransformerF[F, short.NumScale[A, Nothing], long.NumScale[B]] = {
    inline Transformer
      .defineF[F, short.NumScale[A, Nothing], long.NumScale[B]]
      .withCoproductInstance[short.Billion[A], long.Milliard[B]] { billion =>
        billion.transformInto[long.Milliard[B]]
      }
      .withCoproductInstance[short.Trillion[A], long.Billion[B]] { trillion =>
        trillion.transformInto[long.Billion[B]]
      } match
        case definition: TransformerFDefinition[F, short.NumScale[A, Nothing], long.NumScale[B], config, flags] =>
          MacroUtils.showType[DeriveUtils.InstanceConfigOf[config, short.Billion[A]]]
          MacroUtils.showType[DeriveUtils.InstanceConfigOf[config, short.Trillion[A]]]
          definition.buildTransformer
  }

  inline given shortToLongWrappedInner[F[_]: TransformerFSupport, A, B](
      using ft: TransformerF[F, A, B]
  ): TransformerF[F, short.NumScale[A, Nothing], long.NumScale[B]] = {
    inline Transformer
      .defineF[F, short.NumScale[A, Nothing], long.NumScale[B]]
      .withCoproductInstanceF[short.Billion[A], long.Milliard[B]] { billion =>
        billion.transformIntoF[F, long.Milliard[B]]
      }
      .withCoproductInstanceF[short.Trillion[A], long.Billion[B]] { trillion =>
        trillion.transformIntoF[F, long.Billion[B]]
      } match
        case definition: TransformerFDefinition[F, short.NumScale[A, Nothing], long.NumScale[B], config, flags] =>
          MacroUtils.showType[DeriveUtils.InstanceConfigOf[config, short.Billion[A]]]
          MacroUtils.showType[DeriveUtils.InstanceConfigOf[config, short.Trillion[A]]]
          definition.buildTransformer
  }

}
