package io.scalaland.chimney

import io.scalaland.chimney.examples._
import io.scalaland.chimney.dsl._
import io.scalaland.chimney.internal.derived.DeriveUtils
import io.scalaland.chimney.internal.utils.MacroUtils
import utest._

object MockTest extends TestSuite:

  val tests = Tests {
    "TransformerF" - {
      "correctly transforms coproducts" - {
        val shortCases = List[short.NumScale[Int, Nothing]](short.Zero, short.Million(42), short.Billion(420), short.Trillion(9000))
        val expectedLongCases = List(long.Zero, long.Million(42L), long.Milliard(420L), long.Billion(9000L)).map(Some(_))

        println(shortCases.map(_.transformIntoF[Option, long.NumScale[Long]]))
        shortCases.map(_.transformIntoF[Option, long.NumScale[Long]]) ==> expectedLongCases
      }

      "correctly transform coproducts with optional computed instances" - {
        val shortCases = List[short.NumScale[Long, Nothing]](short.Zero, short.Million(42L), short.Billion(420L), short.Trillion(Long.MaxValue))
        val expectedLongCases = List(Some(long.Zero), Some(long.Million(42)), Some(long.Milliard(420)), None)
        shortCases.map(_.transformIntoF[Option, long.NumScale[Int]]) ==> expectedLongCases
      }
    }
  }

  given identityInt: Transformer[Int, Long] with
    def transform(from: Int): Long = from.toLong

  given intLongOpt: TransformerF[Option, Long, Int] with
    def transform(from: Long): Option[Int] = if from.isValidInt then Some(from.toInt) else None

  //for now it is required
  given mockComputed: TransformerF[Option, short.NumScale[Int, Nothing], long.NumScale[Long]] = ScalesTransformer.shortToLongPureInner
  given mockComputedOptional: TransformerF[Option, short.NumScale[Long, Nothing], long.NumScale[Int]] = ScalesTransformer.shortToLongWrappedInner[Option, Long, Int]

end MockTest
