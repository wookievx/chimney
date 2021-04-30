package io.scalaland.chimney

import io.scalaland.chimney.dsl._
import io.scalaland.chimney.examples._
import io.scalaland.chimney.internal.utils.MacroUtils
import io.scalaland.chimney.internal.TransformerFlag
import utest._

object TransformerDslSpec extends TestSuite {

  val tests = Tests {

    "use given instances directly" - {
      import Domain1._

      given instance: Transformer[UserName, String] = userNameToStringTransformer
      
      UserName("Batman").into[String].transform ==> "BatmanT"
      UserName("Batman").transformInto[String] ==> "BatmanT"
    }

    "use given transformer for nested field" - {
      import Domain1._

      given instance: Transformer[UserName, String] = userNameToStringTransformer

      val batman = User("123", UserName("Batman"))
      val batmanDTO = batman.transformInto[UserDTO]

      batmanDTO.id ==> "123"
      batmanDTO.name ==> "BatmanT"
    }
    
    "support different set of fields of source and target" - {

      case class Foo(x: Int, y: String, z: (Double, Double))
      case class Bar(x: Int, z: (Double, Double))
      case class HaveY(y: String)

      "field is dropped - the target" - {
        Foo(3, "pi", (3.14, 3.14)).transformInto[Bar] ==> Bar(3, (3.14, 3.14))
      }

      "field is added to the target" - {

        // "not compile if source for the target fields is not provided" - {

        //   compileError("Bar(3, (3.14, 3.14)).transformInto[Foo]")
        //     .check("", "no accessor named y in source type io.scalaland.chimney.DslSpec.Bar")
        // }

        "fill the field with provided default value" - {

          "pass when selector is valid" - {

            Bar(3, (3.14, 3.14))
              .into[Foo]
              .withFieldConst(_.y, "pi")
              .transform ==>
              Foo(3, "pi", (3.14, 3.14))

            Bar(3, (3.14, 3.14))
              .into[Foo]
              .withFieldConst(cc => cc.y, "pi")
              .transform ==>
              Foo(3, "pi", (3.14, 3.14))
          }

          "not compile when selector is invalid" - {

            compileError("""Bar(3, (3.14, 3.14))
                  .into[Foo]
                  .withFieldConst(_.y, "pi")
                  .withFieldConst(_.z._1, 0.0)
                  .transform
                """)
              .check("", "Illegal selector: ((x: Foo) => x.z._1)")

            compileError("""Bar(3, (3.14, 3.14))
                  .into[Foo]
                  .withFieldConst(_.y + "abc", "pi")
                  .transform
                """)
              .check("", "Illegal selector: ((x: Foo) => x.y.+(\"abc\"))")

            compileError("""
                val haveY = HaveY("")
                Bar(3, (3.14, 3.14))
                  .into[Foo]
                  .withFieldConst(cc => haveY.y, "pi")
                  .transform
                """)
              .check("", "Illegal selector: ((cc: Foo) => haveY.y)")
          }

          "fill the field with provided generator function" - {

            "pass when selector is valid" - {

              Bar(3, (3.14, 3.14))
                .into[Foo]
                .withFieldComputed(_.y, _.x.toString)
                .transform ==> Foo(3, "3", (3.14, 3.14))

              Bar(3, (3.14, 3.14))
                .into[Foo]
                .withFieldComputed(cc => cc.y, _.x.toString)
                .transform ==> Foo(3, "3", (3.14, 3.14))
            }

            "not compile when selector is invalid" - {

              compileError("""Bar(3, (3.14, 3.14))
                  .into[Foo]
                  .withFieldComputed(_.y, _.x.toString)
                  .withFieldComputed(_.z._1, _.z._1 * 10.0)
                  .transform
                """)
                .check("", "Illegal selector: ((x: Foo) => x.z._1)")

              compileError("""Bar(3, (3.14, 3.14))
                  .into[Foo]
                  .withFieldComputed(_.y + "abc", _.x.toString)
                  .transform
                """)
                .check("", "Illegal selector: ((x: Foo) => x.y.+(\"abc\"))")

              compileError("""
                val haveY = HaveY("")
                Bar(3, (3.14, 3.14))
                  .into[Foo]
                  .withFieldComputed(cc => haveY.y, _.x.toString)
                  .transform
                """)
                .check("", "Illegal selector: ((cc: Foo) => haveY.y)")
            }

          }

        }

        //compilation failure specs are not possible at this time, when compilation error is reported deep inside inlined code it is emitted at compiletime
        "support default values for Options" - {

          "use None when .enableOptionDefaultsToNone" - {
            fooToFoobarOptDefault ==> Foobar("foo", None)
          }

          // "not compile if .enableOptionDefaultsToNone is missing" - {
          //   compileError("""SomeFoo("foo").into[Foobar].transform ==> Foobar("foo", None)""")
          //     .check("", "Chimney can't derive transformation from SomeFoo to Foobar")
          // }

          "target has default value, but default values are disabled and .enableOptionDefaultsToNone" - {
            fooToFoobar2OptDefNone ==> Foobar2("foo", None)
          }

          "not use None as default when other default value is set" - {
            fooToFoobar2NoOptDef ==> Foobar2("foo", Some(42))
            fooToFoobar2PrederDefault ==> Foobar2("foo", Some(42))
          }

          // "not compile if default value is missing and no .enableOptionDefaultsToNone" - {
          //   compileError("""SomeFoo("foo").into[Foobar].transform""")
          //     .check("", "Chimney can't derive transformation from SomeFoo to Foobar")
          // }

          // "not compile if default values are disabled and no .enableOptionDefaultsToNone" - {
          //   compileError("""SomeFoo("foo").into[Foobar2].disableDefaultValues.transform""")
          //     .check("", "Chimney can't derive transformation from SomeFoo to Foobar2")
          // }
        }

        "fill the field with provided generator function" - {

          "pass when selector is valid" - {

            Bar(3, (3.14, 3.14))
              .into[Foo]
              .withFieldComputed(_.y, _.x.toString)
              .transform ==>
              Foo(3, "3", (3.14, 3.14))

            Bar(3, (3.14, 3.14))
              .into[Foo]
              .withFieldComputed(cc => cc.y, _.x.toString)
              .transform ==>
              Foo(3, "3", (3.14, 3.14))
          }

          "not compile when selector is invalid" - {

            compileError("""Bar(3, (3.14, 3.14))
                  .into[Foo]
                  .withFieldComputed(_.y, _.x.toString)
                  .withFieldComputed(_.z._1, _.z._1 * 10.0)
                  .transform
                """)
              .check("", "Illegal selector: ((x: Foo) => x.z._1)")

            compileError("""Bar(3, (3.14, 3.14))
                  .into[Foo]
                  .withFieldComputed(_.y + "abc", _.x.toString)
                  .transform
                """)
              .check("", "Illegal selector: ((x: Foo) => x.y.+(\"abc\"))")

            compileError("""
                val haveY = HaveY("")
                Bar(3, (3.14, 3.14))
                  .into[Foo]
                  .withFieldComputed(cc => haveY.y, _.x.toString)
                  .transform
                """)
              .check("", "Illegal selector: ((cc: Foo) => haveY.y)")
          }
        }

      }

    }

    "support default parameters" - {

      "use default parameter value" - {

        "field does not exists - the source" - {
          DefaultSupportSpecs.`field does not exists - the source-single`
          DefaultSupportSpecs.`field does not exists - the source-sequence`
        }

        "field does not exists - nested object" - {
          DefaultSupportSpecs.`field does not exists - nested object`
        }
      }

      "not use default parameter value" - {

        "field exists - the source" - {
          DefaultSupportSpecs.`field exists - the source-single`
          DefaultSupportSpecs.`field exists - the source-sequence`
        }

        "another modifier is provided" - {
          DefaultSupportSpecs.`another modifier is provided`
        }

        "default values are disabled and another modifier is provided" - {
          DefaultSupportSpecs.`default values are disabled and another modifier is provided-one`

          DefaultSupportSpecs.`default values are disabled and another modifier is provided-two`
        }

        "local transformer for default value exists" - {
          DefaultSupportSpecs.`local transformer for default value exists`
        }

        "local transformer for the whole entity exists" - {
          DefaultSupportSpecs.`local transformer for the whole entity exists`
        }
      }

      // the same limitation as above, not able to move compilation errors to runtime

      // "not compile when default parameter values are disabled" - {
      //   compileError("""
      //     Foo(10).into[Bar].disableDefaultValues.transform
      //   """)
      //     .check("", "Chimney can't derive transformation from Foo to Bar")

      //   compileError("""
      //     Baah(10, Foo(300)).into[Baahr].disableDefaultValues.transform
      //   """)
      //     .check("", "Chimney can't derive transformation from Baah to Baahr")
      // }
    }

    "transform with rename" - {

      "between different types: correct" - {
        TransformWithRenameSpecs.`between different types: correct`
      }

      "between different types: incorrect" - {
        TransformWithRenameSpecs.`between different types: incorrect`
      }

      // "between different types: without implicit" - {
      //   compileError("""
      //       val user: User = User(1, "Kuba", None)
      //       user.into[UserPL].withFieldRenamed(_.name, _.imie)
      //           .withFieldRenamed(_.age, _.wiek)
      //           .transform
      //     """)
      //     .check("", "Chimney can't derive transformation from User to UserPL")
      // }
    }

    "support relabelling of fields" - {

      // again not possible to elegantly check this

      // "not compile if relabelling modifier is not provided" - {

      //   compileError("""Foo(10, "something").transformInto[Bar]""")
      //     .check("", "Chimney can't derive transformation from Foo to Bar")
      // }

      "relabel fields with relabelling modifier" - {
        RelabelingOfFieldSpec.`relabel fields with relabelling modifier`
      }

      "not compile if relabelling selectors are invalid" - {
        import RelabelingOfFieldSpec._

        compileError("""
            Foo(10, "something")
              .into[Bar]
              .withFieldRenamed(_.y + "abc", _.z)
              .transform
          """)
          .check("", """Illegal selector: ((x: io.scalaland.chimney.TransformerDslSpec.RelabelingOfFieldSpec.Foo) => x.y.+("abc"))""")

        compileError("""
            val haveY = HaveY("")
            Foo(10, "something")
              .into[Bar]
              .withFieldRenamed(cc => haveY.y, _.z)
              .transform
          """)
          .check("", """Illegal selector: ((cc: io.scalaland.chimney.TransformerDslSpec.RelabelingOfFieldSpec.Foo) => haveY.y)""")

        compileError("""
            Foo(10, "something")
              .into[Bar]
              .withFieldRenamed(_.y, _.z + "abc")
              .transform
          """)
          .check("", """Illegal selector: ((x: io.scalaland.chimney.TransformerDslSpec.RelabelingOfFieldSpec.Bar) => x.z.+("abc")""")

        compileError("""
            val haveZ = HaveZ("")
            Foo(10, "something")
              .into[Bar]
              .withFieldRenamed(_.y, cc => haveZ.z)
              .transform
          """)
          .check("", """Illegal selector: ((cc: io.scalaland.chimney.TransformerDslSpec.RelabelingOfFieldSpec.Bar) => haveZ.z)""")

        compileError("""
            Foo(10, "something")
              .into[Bar]
              .withFieldRenamed(_.y + "abc", _.z + "abc")
              .transform
          """)
          .check("", """Illegal selector: ((x: io.scalaland.chimney.TransformerDslSpec.RelabelingOfFieldSpec.Foo) => x.y.+("abc"))""")

        compileError("""
            val haveY = HaveY("")
            val haveZ = HaveZ("")
            Foo(10, "something")
              .into[Bar]
              .withFieldRenamed(cc => haveY.y, cc => haveZ.z)
              .transform
          """)
          .check("", """Illegal selector: ((cc: io.scalaland.chimney.TransformerDslSpec.RelabelingOfFieldSpec.Foo) => haveY.y)""")
      }

      // again not possible to elegantly check this

      // "not compile if relabelled - a wrong way" - {

      //   compileError("""Foo(10, "something").into[Bar].withFieldRenamed(_.y, _.x).transform""")
      //     .check("", "Chimney can't derive transformation from Foo to Bar")

      //   compileError("""Foo(10, "something").into[Bar].withFieldRenamed(_.x, _.z).transform""")
      //     .check("", "Chimney can't derive transformation from Foo to Bar")
      // }

    }

    "support common data-types" - {
      import CommonDataTypesSpec._

      "support scala.Option" - {
        Option(Foo("a")).transformInto[Option[Bar]] ==> Option(Bar("a"))
        (Some(Foo("a")): Option[Foo]).transformInto[Option[Bar]] ==> Option(Bar("a"))
        Some(Foo("a")).transformInto[Option[Bar]] ==> Some(Bar("a"))
        (None: Option[Foo]).transformInto[Option[Bar]] ==> None
        (None: Option[String]).transformInto[Option[String]] ==> None
        Option("abc").transformInto[Option[String]] ==> Some("abc")
      }

      "support scala.util.Either" - {
        (Left(Foo("a")): Either[Foo, Foo]).transformInto[Either[Bar, Bar]] ==> Left(Bar("a"))
        (Right(Foo("a")): Either[Foo, Foo]).transformInto[Either[Bar, Bar]] ==> Right(Bar("a"))
        // Left(Foo("a")).transformInto[Either[Bar, Bar]] ==> Left(Bar("a"))
        // Right(Foo("a")).transformInto[Either[Bar, Bar]] ==> Right(Bar("a"))
        // Left(Foo("a")).transformInto[Left[Bar, Bar]] ==> Left(Bar("a"))
        // Right(Foo("a")).transformInto[Right[Bar, Bar]] ==> Right(Bar("a"))
        (Left("a"): Either[String, String]).transformInto[Either[String, String]] ==> Left("a")
        (Right("a"): Either[String, String]).transformInto[Either[String, String]] ==> Right("a")
      }

      "support Iterables collections" - {
        Seq(Foo("a")).transformInto[Seq[Bar]] ==> Seq(Bar("a"))
        List(Foo("a")).transformInto[List[Bar]] ==> List(Bar("a"))
        Vector(Foo("a")).transformInto[Vector[Bar]] ==> Vector(Bar("a"))
        Set(Foo("a")).transformInto[Set[Bar]] ==> Set(Bar("a"))

        Seq("a").transformInto[Seq[String]] ==> Seq("a")
        List("a").transformInto[List[String]] ==> List("a")
        Vector("a").transformInto[Vector[String]] ==> Vector("a")
        Set("a").transformInto[Set[String]] ==> Set("a")

        List(Foo("a")).transformInto[Seq[Bar]] ==> Seq(Bar("a"))
        Vector(Foo("a")).transformInto[Seq[Bar]] ==> Seq(Bar("a"))

        List("a").transformInto[Seq[String]] ==> Seq("a")
        Vector("a").transformInto[Seq[String]] ==> Seq("a")
      }

      "support Arrays" - {
        Array(Foo("a")).transformInto[Array[Foo]] ==> Array(Foo("a"))
        Array(Foo("a")).transformInto[Array[Bar]] ==> Array(Bar("a"))
        Array("a").transformInto[Array[String]] ==> Array("a")
      }

      "support conversion between Iterables and Arrays" - {

        Array(Foo("a")).transformInto[List[Bar]] ==> List(Bar("a"))
        Array("a", "b").transformInto[Seq[String]] ==> Seq("a", "b")
        Array(3, 2, 1).transformInto[Vector[Int]] ==> Vector(3, 2, 1)

        Vector("a").transformInto[Array[String]] ==> Array("a")
        List(1, 6, 3).transformInto[Array[Int]] ==> Array(1, 6, 3)
        Seq(Bar("x"), Bar("y")).transformInto[Array[Foo]] ==> Array(Foo("x"), Foo("y"))
      }

      "support Map" - {
        Map("test" -> Foo("a")).transformInto[Map[String, Bar]] ==> Map("test" -> Bar("a"))
        Map("test" -> "a").transformInto[Map[String, String]] ==> Map("test" -> "a")
        Map(Foo("test") -> "x").transformInto[Map[Bar, String]] ==> Map(Bar("test") -> "x")
        Map(Foo("test") -> Foo("x")).transformInto[Map[Bar, Bar]] ==> Map(Bar("test") -> Bar("x"))
      }

      // "support conversion between Iterables and Maps" - {

      //   Seq(Foo("10") -> Bar("20"), Foo("20") -> Bar("40")).transformInto[Map[Bar, Foo]] ==>
      //     Map(Bar("10") -> Foo("20"), Bar("20") -> Foo("40"))

      //   Map(Foo("10") -> Bar("20"), Foo("20") -> Bar("40")).transformInto[List[(Bar, Foo)]] ==>
      //     List(Bar("10") -> Foo("20"), Bar("20") -> Foo("40"))
      // }

      // "support conversion between Arrays and Maps" - {

      //   Array(Foo("10") -> Bar("20"), Foo("20") -> Bar("40")).transformInto[Map[Bar, Foo]] ==>
      //     Map(Bar("10") -> Foo("20"), Bar("20") -> Foo("40"))

      //   Map(Foo("10") -> Bar("20"), Foo("20") -> Bar("40")).transformInto[Array[(Bar, Foo)]] ==>
      //     Array(Bar("10") -> Foo("20"), Bar("20") -> Foo("40"))
      // }

    }

  }

  //workaround scala3 bugs, hopefully will be fixed one day
  case class SomeFoo(x: String)
  case class Foobar(x: String, y: Option[Int])
  case class Foobar2(x: String, y: Option[Int] = Some(42))

  lazy val fooToFoobarOptDefault = SomeFoo("foo").into[Foobar].enableOptionDefaultsToNone.transform
  lazy val fooToFoobar2OptDefNone = SomeFoo("foo").into[Foobar2].disableDefaultValues.enableOptionDefaultsToNone.transform
  lazy val fooToFoobar2NoOptDef = SomeFoo("foo").into[Foobar2].transform
  lazy val fooToFoobar2PrederDefault = SomeFoo("foo").into[Foobar2].enableOptionDefaultsToNone.transform

  object DefaultSupportSpecs {
    case class Foo(x: Int)
    case class Bar(x: Int, y: Long = 30L)
    case class Baz(x: Int = 5, y: Long = 100L)
    case class Baah(x: Int, y: Foo = Foo(0))
    case class Baahr(x: Int, y: Bar)

    def `field does not exists - the source-single` = Foo(10).transformInto[Bar] ==> Bar(10, 30L)
    def `field does not exists - the source-sequence` = Seq(Foo(30), Foo(40)).transformInto[Seq[Bar]] ==> Seq(Bar(30, 30L), Bar(40, 30L))

    def `field does not exists - nested object` = Baah(10, Foo(300)).transformInto[Baahr] ==> Baahr(10, Bar(300, 30L))

    def `field exists - the source-single` = Bar(100, 200L).transformInto[Baz] ==> Baz(100, 200L)
    def `field exists - the source-sequence` = Seq(Bar(100, 200L), Bar(300, 400L)).transformInto[Seq[Baz]] ==> Seq(Baz(100, 200L), Baz(300, 400L))

    def `another modifier is provided` = Foo(10).into[Bar].withFieldConst(_.y, 45L).transform ==> Bar(10, 45L) 

    def `default values are disabled and another modifier is provided-one` =
      Foo(10).into[Bar].disableDefaultValues.withFieldConst(_.y, 45L).transform ==> Bar(10, 45L)

    def `default values are disabled and another modifier is provided-two` =
      Foo(10).into[Bar].withFieldConst(_.y, 48L).disableDefaultValues.transform ==> Bar(10, 48L)

    def `local transformer for default value exists` =
      given localTransformer: Transformer[Long, Foo] with
        def transform(from: Long): Foo = Foo(from.toInt * 10)          
      
      Bar(100, 300L).transformInto[Baah] ==> Baah(100, Foo(3000))


    def `local transformer for the whole entity exists` =
      given fooBarTransformer: Transformer[Foo, Bar] with
        def transform(from: Foo): Bar = Bar(from.x, 333L)
      
      Foo(333).transformInto[Bar] ==> Bar(333, 333L)

  }

  object TransformWithRenameSpecs {
    case class User(id: Int, name: String, age: Option[Int])
    case class UserPL(id: Int, imie: String, wiek: Either[Unit, Int])
    def ageToWiekTransformer: Transformer[Option[Int], Either[Unit, Int]] =
      new Transformer[Option[Int], Either[Unit, Int]] {
        def transform(obj: Option[Int]): Either[Unit, Int] =
         obj.fold[Either[Unit, Int]](Left(()))(Right.apply)
      }

    def `between different types: correct` =
      given trans: Transformer[Option[Int], Either[Unit, Int]] = ageToWiekTransformer

      val user: User = User(1, "Kuba", Some(28))
      val userPl = UserPL(1, "Kuba", Right(28))
      user
        .into[UserPL]
        .withFieldRenamed(_.name, _.imie)
        .withFieldRenamed(_.age, _.wiek)
        .transform ==> userPl

    def `between different types: incorrect` =  
      given trans: Transformer[Option[Int], Either[Unit, Int]] = ageToWiekTransformer

      val user: User = User(1, "Kuba", None)
      val userPl = UserPL(1, "Kuba", Left(()))
      user
        .into[UserPL]
        .withFieldRenamed(_.name, _.imie)
        .withFieldRenamed(_.age, _.wiek)
        .transform ==> userPl
    
  }

  object RelabelingOfFieldSpec {
    case class Foo(x: Int, y: String)
    case class Bar(x: Int, z: String)
    case class HaveY(y: String)
    case class HaveZ(z: String)

    def `relabel fields with relabelling modifier` = Foo(10, "something").into[Bar].withFieldRenamed(_.y, _.z).transform ==> Bar(10, "something")

  }

  object CommonDataTypesSpec {
    case class Foo(value: String)
    case class Bar(value: String)
  }


}

object Domain1 {

  case class UserName(value: String)

  val userNameToStringTransformer: Transformer[UserName, String] =
    (userName: UserName) => userName.value + "T"

  case class UserDTO(id: String, name: String)

  case class User(id: String, name: UserName)

}

object VCDomain1 {

  case class UserName(value: String) extends AnyVal

  case class UserDTO(id: String, name: String)

  case class User(id: String, name: UserName)

}

object Poly {

  case class MonoSource(poly: String, other: String)

  case class PolySource[T](poly: T, other: String)

  case class MonoTarget(poly: String, other: String)

  case class PolyTarget[T](poly: T, other: String)

  val monoSource = MonoSource("test", "test")
  val polySource = PolySource("test", "test")
  val monoTarget = MonoTarget("test", "test")
  val polyTarget = PolyTarget("test", "test")
}
