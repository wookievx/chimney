package io.scalaland.chimney

import io.scalaland.chimney.examples.*
import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.internal.derived.DeriveUtils
import io.scalaland.chimney.internal.utils.MacroUtils
import ScalesTransformer.given
import utest.*

object MockTest extends TestSuite:

  val tests = Tests {
    "TransformerF" - {
      "correctly transforms coproducts" - {
        val shortCases = List[short.NumScale[Int, Nothing]](short.Zero, short.Million(42), short.Billion(420), short.Trillion(9000), short.Infinity)
        val expectedLongCases = List(long.Zero, long.Million(42L), long.Milliard(420L), long.Billion(9000L), long.Infinity).map(Some(_))
        shortCases.map(_.transformIntoF[Option, long.NumScale[Long]]) ==> expectedLongCases
      }

      "correctly transform coproducts with optional computed instances" - {
        val shortCases = List[short.NumScale[Long, Nothing]](short.Zero, short.Million(42L), short.Billion(420L), short.Trillion(Long.MaxValue), short.Infinity)
        val expectedLongCases = List(Some(long.Zero), Some(long.Million(42)), Some(long.Milliard(420)), None, Some(long.Infinity))
        shortCases.map(_.transformIntoF[Option, long.NumScale[Int]]) ==> expectedLongCases
      }
    }

    "Transformer" - {
      "correctly derive definition with new macros" - {
        import TransformerDslSpec.RelabelingOfFieldSpec.*
        import internal.derived.TransformerDerive
        val t = TransformerDerive.derived(defaultDefinition[Foo, Bar].withFieldRenamed(_.y, _.z))
        val tComputed = TransformerDerive.derived(defaultDefinition[Foo, Bar].withFieldComputed(_.z, foo => s"Got $foo"))
        t.transform(Foo(10, "something")) ==> Bar(10, "something")
        tComputed.transform(Foo(10, "something")) ==> Bar(10, "Got Foo(10,something)")
      }

      "correctly convert collections with new macros" - {
        import TransformerDslSpec.RelabelingOfFieldSpec.*
        import internal.derived.TransformerDerive
        val t = TransformerDerive.derived(defaultDefinition[List[Foo], Array[Foo]])
        t.transform(List(Foo(10, "something"), Foo(20, "nothing"))) ==> Array(Foo(10, "something"), Foo(20, "nothing"))
      }

      "correctly convert enums" - {
        import internal.derived.TransformerDerive

        val t = exampleEnum

        t.transform(Source.First("Anything")) ==> Target.Third(42)
        t.transform(Source.Second(420)) ==> Target.Second(420)
      }
    }
  }

  given identityInt: Transformer[Int, Long] with
    def transform(from: Int): Long = from.toLong

  given intLongOpt: TransformerF[Option, Long, Int] with
    def transform(from: Long): Option[Int] = if from.isValidInt then Some(from.toInt) else None


  val compilationTest = {
    import TransformerDslSpec.RelabelingOfFieldSpec.*

    Foo(10, "something").into[Bar].withFieldRenamed(_.y, _.z).transform ==> Bar(10, "something")
    Foo(10, "something").into[Bar].withFieldRenamed(_.y, _.z).transform ==> Bar(10, "something")
    Foo(10, "something").into[Bar].withFieldRenamed(_.y, _.z).transform ==> Bar(10, "something")
    Foo(10, "something").into[Bar].withFieldRenamed(_.y, _.z).transform ==> Bar(10, "something")
    Foo(10, "something").into[Bar].withFieldRenamed(_.y, _.z).transform ==> Bar(10, "something")
    Foo(10, "something").into[Bar].withFieldRenamed(_.y, _.z).transform ==> Bar(10, "something")
    Foo(10, "something").into[Bar].withFieldRenamed(_.y, _.z).transform ==> Bar(10, "something")
    Foo(10, "something").into[Bar].withFieldRenamed(_.y, _.z).transform ==> Bar(10, "something")
    Foo(10, "something").into[Bar].withFieldRenamed(_.y, _.z).transform ==> Bar(10, "something")
    Foo(10, "something").into[Bar].withFieldRenamed(_.y, _.z).transform ==> Bar(10, "something")
    Foo(10, "something").into[Bar].withFieldRenamed(_.y, _.z).transform ==> Bar(10, "something")
    Foo(10, "something").into[Bar].withFieldRenamed(_.y, _.z).transform ==> Bar(10, "something")
    Foo(10, "something").into[Bar].withFieldRenamed(_.y, _.z).transform ==> Bar(10, "something")
    Foo(10, "something").into[Bar].withFieldRenamed(_.y, _.z).transform ==> Bar(10, "something")
    Foo(10, "something").into[Bar].withFieldRenamed(_.y, _.z).transform ==> Bar(10, "something")
    Foo(10, "something").into[Bar].withFieldRenamed(_.y, _.z).transform ==> Bar(10, "something")
    Foo(10, "something").into[Bar].withFieldRenamed(_.y, _.z).transform ==> Bar(10, "something")
    Foo(10, "something").into[Bar].withFieldRenamed(_.y, _.z).transform ==> Bar(10, "something")
    Foo(10, "something").into[Bar].withFieldRenamed(_.y, _.z).transform ==> Bar(10, "something")

  }

  enum Source:
    case First(field: String)
    case Second(field: Int)
  end Source

  enum Target:
    case Second(field: Int)
    case Third(small: Short)
  end Target

  def exampleEnum = {
    import internal.derived.TransformerDerive
    TransformerDerive.derived(defaultDefinition[Source, Target].withCoproductInstance[Source.First, Target.Third](_ => Target.Third(42)))
  }

end MockTest
