package io.scalaland.chimney

import io.scalaland.chimney.dsl._
import io.scalaland.chimney.examples._
import io.scalaland.chimney.internal.utils.MacroUtils
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
              .check("", "Illegal selector: ((_$6: Foo) => _$6.z._1)")

            compileError("""Bar(3, (3.14, 3.14))
                  .into[Foo]
                  .withFieldConst(_.y + "abc", "pi")
                  .transform
                """)
              .check("", "Illegal selector: ((_$7: Foo) => _$7.y.+(\"abc\"))")

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
                .check("", "Illegal selector: ((_$10: Foo) => _$10.z._1)")

              compileError("""Bar(3, (3.14, 3.14))
                  .into[Foo]
                  .withFieldComputed(_.y + "abc", _.x.toString)
                  .transform
                """)
                .check("", "Illegal selector: ((_$12: Foo) => _$12.y.+(\"abc\"))")

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

    }

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
