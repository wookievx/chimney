package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.examples.*
import io.scalaland.chimney.examples.trip.*
import io.scalaland.chimney.internal.utils.MacroUtils
import io.scalaland.chimney.internal.TransformerFlag
import utest.*
import scala.collection.mutable.{Queue, ArrayBuffer}

import io.scalaland.chimney.utils.EitherUtils.*
import io.scalaland.chimney.utils.OptionUtils.*

object TransformerDslFSpec extends TestSuite {

  val tests = Tests {

    "transform always succeeds" - {

      "option" - {
        Person("John", 10, 140).transformIntoF[Option, User] ==> Some(
          User("John", 10, 140)
        )
      }

      "either" - {
        Person("John", 10, 140)
          .transformIntoF[[t] =>> Either[Vector[String], t], User] ==> Right(
          User("John", 10, 140)
        )
      }

    }

    "transform always fails" - {

      "option" - {
        Person("John", 10, 140)
          .intoF[Option, User]
          .withFieldConstF(_.height, Option.empty[Double])
          .transform ==> None
      }

      "either" - {
        Person("John", 10, 140)
          .intoF[[t] =>> Either[List[String], t], User]
          .withFieldConstF(_.height, Left(List("abc", "def")))
          .transform ==> Left(List("abc", "def"))
      }
    }

    "simple transform with validation" - {

      "success" - {
        val okForm = PersonForm("John", "10", "140")

        "1-arg" - {

          "option" - {
            okForm
              .into[Person]
              .withFieldConst(_.age, 7)
              .withFieldComputedF(_.height, _.height.parseDouble)
              .transform ==> Some(Person("John", 7, 140))
          }

          "either" - {
            okForm
              .into[Person]
              .withFieldConst(_.height, 200.5)
              .withFieldComputedF(_.age, _.age.parseInt.toEither("bad age"))
              .transform ==> Right(Person("John", 10, 200.5))
          }
        }

        "2-arg" - {

          "option" - {
            okForm
              .into[Person]
              .withFieldComputedF(_.age, _.age.parseInt)
              .withFieldComputedF(_.height, _.height.parseDouble)
              .transform ==> Some(Person("John", 10, 140))
          }

          "either" - {
            okForm
              .intoF[[t] =>> Either[List[String], t], Person]
              .withFieldConst(_.name, "Joe")
              .withFieldComputedF(
                _.height,
                _.height.parseDouble.toEither("bad height")
              )
              .withFieldComputedF(_.age, _.age.parseInt.toEither("bad age"))
              .transform ==> Right(Person("Joe", 10, 140))
          }
        }

        "3-arg" - {

          "option" - {
            okForm
              .into[Person]
              .withFieldComputedF(
                _.name,
                pf =>
                  if (pf.name.isEmpty) None
                  else Some(pf.name.toUpperCase())
              )
              .withFieldComputedF(_.age, _.age.parseInt)
              .withFieldComputedF(_.height, _.height.parseDouble)
              .transform ==> Some(Person("JOHN", 10, 140))
          }

          "either" - {
            okForm
              .intoF[[t] =>> Either[List[String], t], Person]
              .withFieldComputedF(
                _.name,
                pf =>
                  if (pf.name.isEmpty) Left(List("empty name"))
                  else Right(pf.name.toUpperCase())
              )
              .withFieldComputedF(_.age, _.age.parseInt.toEither("bad age"))
              .withFieldComputedF(
                _.height,
                _.height.parseDouble.toEither("bad height")
              )
              .transform ==> Right(Person("JOHN", 10, 140))
          }
        }
      }

      "failure with error handling" - {
        val badForm = PersonForm("", "foo", "bar")

        "option" - {
          badForm
            .intoF[Option, Person]
            .withFieldComputedF(_.age, _.age.parseInt)
            .withFieldComputedF(_.height, _.age.parseDouble)
            .transform ==> None
        }

        "either" - {
          badForm
            .into[Person]
            .withFieldComputedF(
              _.name,
              pf =>
                if (pf.name.isEmpty) Left(List("empty name"))
                else Right(pf.name.toUpperCase())
            )
            .withFieldComputedF(_.age, _.age.parseInt.toEither("bad age"))
            .withFieldComputedF(
              _.height,
              _.age.parseDouble.toEither("bad double")
            )
            .transform ==> Left(
            List("empty name")
          ) //really don't want to accumulate for now, support instance would need to be accumulating, and it is not
        }
      }

    }

    "wrapped options" - {
      type E = [t] =>> Either[List[String], t]

      "pure inner transformer" - {

        given intPrinter: Transformer[Int, String] = _.toString

        "F = Option" - {
          Option(123).transformIntoF[Option, Option[String]] ==> Some(
            Some("123")
          )
          Option.empty[Int].transformIntoF[Option, Option[String]] ==> Some(
            None
          )
        }

        "F = Either[List[String], +*]]" - {
          Option(123).transformIntoF[E, Option[String]] ==> Right(Some("123"))
          Option.empty[Int].transformIntoF[E, Option[String]] ==> Right(None)
        }
      }

      "wrapped inner transformer" - {

        "F = Option" - {
          given intParserOpt: TransformerF[Option, String, Int] = _.parseInt

          Option("123").transformIntoF[Option, Option[Int]] ==> Some(Some(123))
          Option("abc").transformIntoF[Option, Option[Int]] ==> None
          Option.empty[String].transformIntoF[Option, Option[Int]] ==> Some(
            None
          )
        }

        "F = Either[List[String], +*]]" - {

          given intParserEither: TransformerF[E, String, Int] =
            _.parseInt.toEither("bad int")

          Option("123").transformIntoF[E, Option[Int]] ==> Right(Some(123))
          Option("abc").transformIntoF[E, Option[Int]] ==> Left(List("bad int"))
          Option.empty[String].transformIntoF[E, Option[Int]] ==> Right(None)
        }
      }
    }

    "wrapped T to Option[T]" - {
      type E = [t] =>> Either[List[String], t]

      "pure inner transformer" - {

        given intPrinter: Transformer[Int, String] = _.toString

        "F = Option" - {
          10.transformIntoF[Option, Option[String]] ==> Some(Some("10"))
          (null: String).transformIntoF[Option, Option[String]] ==> None
        }

        "F = Either[List[String], +*]]" - {
          10.transformIntoF[E, Option[String]] ==> Right(Some("10"))
          (null: String).transformIntoF[E, Option[String]] ==> Right(None)
        }
      }

      "wrapped inner transformer" - {

        "F = Option" - {
          given intParserOpt: TransformerF[Option, String, Int] = _.parseInt

          "10".transformIntoF[Option, Option[Int]] ==> Some(Some(10))
          (null: String).transformIntoF[Option, Option[Int]] ==> None
          "x".transformIntoF[Option, Option[Int]] ==> None
        }

        "F = Either[List[String], +*]]" - {
          given intParserEither: TransformerF[E, String, Int] =
            _.parseInt.toEither("bad int")

          "10".transformIntoF[E, Option[Int]] ==> Right(Some(10))
          (null: String).transformIntoF[E, Option[Int]] ==> Left(
            List("bad int")
          )
          "x".transformIntoF[E, Option[Int]] ==> Left(List("bad int"))
        }
      }
    }

    "wrapped iterables or arrays" - {
      type E = [t] =>> Either[List[String], t]

      "pure inner transformer" - {

        implicit val intPrinter: Transformer[Int, String] = _.toString

        "F = Option" - {
          List(123, 456).transformIntoF[Option, List[String]] ==> Some(
            List("123", "456")
          )
          Vector(123, 456).transformIntoF[Option, Queue[String]] ==> Some(
            Queue("123", "456")
          )
          Array.empty[Int].transformIntoF[Option, Seq[String]] ==> Some(
            Seq.empty[String]
          )
        }

        "F = Either[List[String], +*]]" - {
          List(123, 456).transformIntoF[E, List[String]] ==> Right(
            List("123", "456")
          )
          Vector(123, 456).transformIntoF[E, Queue[String]] ==> Right(
            Queue("123", "456")
          )
          Array.empty[Int].transformIntoF[E, Seq[String]] ==> Right(
            Seq.empty[String]
          )
        }
      }

      "wrapped inner transformer" - {

        "F = Option" - {
          implicit val intParserOpt: TransformerF[Option, String, Int] =
            _.parseInt

          List("123", "456").transformIntoF[Option, List[Int]] ==> Some(
            List(123, 456)
          )
          Vector("123", "456").transformIntoF[Option, Queue[Int]] ==> Some(
            Queue(123, 456)
          )
          Array.empty[String].transformIntoF[Option, Seq[Int]] ==> Some(
            Seq.empty[Int]
          )
          Set("123", "456")
            .transformIntoF[Option, Array[Int]]
            .get
            .sorted ==> Array(123, 456)

          List("abc", "456").transformIntoF[Option, List[Int]] ==> None
          Vector("123", "def").transformIntoF[Option, Queue[Int]] ==> None
          Array("abc", "def").transformIntoF[Option, Seq[Int]] ==> None
          Set("123", "xyz").transformIntoF[Option, Array[Int]] ==> None
        }

        "F = Either[List[String], +*]]" - {
          given intParserEither
            : TransformerF[[t] =>> Either[List[String], t], String, Int] =
            _.parseInt.toEither("bad int")

          List("123", "456").transformIntoF[E, List[Int]] ==> Right(
            List(123, 456)
          )
          Vector("123", "456").transformIntoF[E, Queue[Int]] ==> Right(
            Queue(123, 456)
          )
          Array.empty[String].transformIntoF[E, Seq[Int]] ==> Right(
            Seq.empty[Int]
          )
          Set("123", "456")
            .transformIntoF[E, Array[Int]]
            .toOption
            .get
            .sorted ==> Array(123, 456)

          List("abc", "456").transformIntoF[E, List[Int]] ==> Left(
            List("bad int")
          )
          Vector("123", "def").transformIntoF[E, Queue[Int]] ==> Left(
            List("bad int")
          )
          Array("abc", "def").transformIntoF[E, Seq[Int]] ==> Left(
            List("bad int")
          )
          Set("123", "xyz").transformIntoF[E, Array[Int]] ==> Left(
            List("bad int")
          )
        }
      }
    }

    "wrapped maps" - {
      type E = [t] =>> Either[List[String], t]

      "pure inner transformer" - {

        given intPrinter: Transformer[Int, String] = _.toString

        "F = Option" - {
          Map(1 -> 10, 2 -> 20)
            .transformIntoF[Option, Map[String, String]] ==> Some(
            Map("1" -> "10", "2" -> "20")
          )
          Map(1 -> 10, 2 -> 20)
            .transformIntoF[Option, Map[String, Int]] ==> Some(
            Map("1" -> 10, "2" -> 20)
          )
          Seq(1 -> 10, 2 -> 20)
            .transformIntoF[Option, Map[String, String]] ==> Some(
            Map("1" -> "10", "2" -> "20")
          )
          ArrayBuffer(1 -> 10, 2 -> 20)
            .transformIntoF[Option, Map[Int, String]] ==> Some(
            Map(1 -> "10", 2 -> "20")
          )
          Map(1 -> 10, 2 -> 20)
            .transformIntoF[Option, List[(String, String)]] ==> Some(
            List("1" -> "10", "2" -> "20")
          )
          Map(1 -> 10, 2 -> 20)
            .transformIntoF[Option, Vector[(String, Int)]] ==> Some(
            Vector("1" -> 10, "2" -> 20)
          )
          Array(1 -> 10, 2 -> 20)
            .transformIntoF[Option, Map[String, String]] ==> Some(
            Map("1" -> "10", "2" -> "20")
          )
          Array(1 -> 10, 2 -> 20)
            .transformIntoF[Option, Map[Int, String]] ==> Some(
            Map(1 -> "10", 2 -> "20")
          )
          Map(1 -> 10, 2 -> 20)
            .transformIntoF[Option, Array[(String, String)]]
            .get ==> Array("1" -> "10", "2" -> "20")
          Map(1 -> 10, 2 -> 20)
            .transformIntoF[Option, Array[(String, Int)]]
            .get ==> Array("1" -> 10, "2" -> 20)
        }

        "F = Either[List[String], +*]]" - {
          Map(1 -> 10, 2 -> 20).transformIntoF[E, Map[String, String]] ==>
            Right(Map("1" -> "10", "2" -> "20"))
          Map(1 -> 10, 2 -> 20).transformIntoF[E, Map[String, Int]] ==>
            Right(Map("1" -> 10, "2" -> 20))
          Seq(1 -> 10, 2 -> 20).transformIntoF[E, Map[String, String]] ==>
            Right(Map("1" -> "10", "2" -> "20"))
          ArrayBuffer(1 -> 10, 2 -> 20).transformIntoF[E, Map[Int, String]] ==>
            Right(Map(1 -> "10", 2 -> "20"))
          Map(1 -> 10, 2 -> 20).transformIntoF[E, List[(String, String)]] ==>
            Right(List("1" -> "10", "2" -> "20"))
          Map(1 -> 10, 2 -> 20).transformIntoF[E, Vector[(String, Int)]] ==>
            Right(Vector("1" -> 10, "2" -> 20))
          Array(1 -> 10, 2 -> 20).transformIntoF[E, Map[String, String]] ==>
            Right(Map("1" -> "10", "2" -> "20"))
          Array(1 -> 10, 2 -> 20).transformIntoF[E, Map[Int, String]] ==>
            Right(Map(1 -> "10", 2 -> "20"))
          Map(1 -> 10, 2 -> 20)
            .transformIntoF[E, Array[(String, String)]]
            .toOption
            .get ==>
            Array("1" -> "10", "2" -> "20")
          Map(1 -> 10, 2 -> 20)
            .transformIntoF[E, Array[(String, Int)]]
            .toOption
            .get ==>
            Array("1" -> 10, "2" -> 20)
        }
      }

      "wrapped inner transformer" - {

        "F = Option" - {
          given intParserOpt: TransformerF[Option, String, Int] = _.parseInt

          Map("1" -> "10", "2" -> "20")
            .transformIntoF[Option, Map[Int, Int]] ==> Some(
            Map(1 -> 10, 2 -> 20)
          )
          Map("1" -> "10", "2" -> "20")
            .transformIntoF[Option, Map[Int, String]] ==> Some(
            Map(1 -> "10", 2 -> "20")
          )
          Seq("1" -> "10", "2" -> "20")
            .transformIntoF[Option, Map[Int, Int]] ==> Some(
            Map(1 -> 10, 2 -> 20)
          )
          ArrayBuffer("1" -> "10", "2" -> "20")
            .transformIntoF[Option, Map[String, Int]] ==>
            Some(Map("1" -> 10, "2" -> 20))
          Map("1" -> "10", "2" -> "20")
            .transformIntoF[Option, List[(Int, Int)]] ==> Some(
            List(1 -> 10, 2 -> 20)
          )
          Map("1" -> "10", "2" -> "20")
            .transformIntoF[Option, Vector[(Int, String)]] ==>
            Some(Vector(1 -> "10", 2 -> "20"))
          Array("1" -> "10", "2" -> "20")
            .transformIntoF[Option, Map[Int, Int]] ==> Some(
            Map(1 -> 10, 2 -> 20)
          )
          Array("1" -> "10", "2" -> "20")
            .transformIntoF[Option, Map[String, Int]] ==> Some(
            Map("1" -> 10, "2" -> 20)
          )
          Map("1" -> "10", "2" -> "20")
            .transformIntoF[Option, Array[(Int, Int)]]
            .get ==> Array(1 -> 10, 2 -> 20)
          Map("1" -> "10", "2" -> "20")
            .transformIntoF[Option, Array[(Int, String)]]
            .get ==> Array(1 -> "10", 2 -> "20")

          Map("1" -> "x", "y" -> "20")
            .transformIntoF[Option, Map[Int, Int]] ==> None
          Map("x" -> "10", "2" -> "20")
            .transformIntoF[Option, Map[Int, String]] ==> None
          Seq("1" -> "10", "2" -> "x")
            .transformIntoF[Option, Map[Int, Int]] ==> None
          ArrayBuffer("1" -> "x", "2" -> "y")
            .transformIntoF[Option, Map[String, Int]] ==> None
          Map("x" -> "10", "y" -> "z")
            .transformIntoF[Option, List[(Int, Int)]] ==> None
          Map("1" -> "10", "x" -> "20")
            .transformIntoF[Option, Vector[(Int, String)]] ==> None
          Array("x" -> "y", "z" -> "v")
            .transformIntoF[Option, Map[Int, Int]] ==> None
          Array("1" -> "x", "2" -> "y")
            .transformIntoF[Option, Map[String, Int]] ==> None
          Map("1" -> "10", "x" -> "20")
            .transformIntoF[Option, Array[(Int, Int)]] ==> None
          Map("x" -> "10", "y" -> "20")
            .transformIntoF[Option, Array[(Int, String)]] ==> None
        }

        "F = Either[List[String], +*]]" - {
          given intParserEither: TransformerF[E, String, Int] =
            _.parseInt.toEither("bad int")

          Map("1" -> "10", "2" -> "20").transformIntoF[E, Map[Int, Int]] ==>
            Right(Map(1 -> 10, 2 -> 20))
          Map("1" -> "10", "2" -> "20").transformIntoF[E, Map[Int, String]] ==>
            Right(Map(1 -> "10", 2 -> "20"))
          Seq("1" -> "10", "2" -> "20").transformIntoF[E, Map[Int, Int]] ==>
            Right(Map(1 -> 10, 2 -> 20))
          ArrayBuffer("1" -> "10", "2" -> "20")
            .transformIntoF[E, Map[String, Int]] ==>
            Right(Map("1" -> 10, "2" -> 20))
          Map("1" -> "10", "2" -> "20").transformIntoF[E, List[(Int, Int)]] ==>
            Right(List(1 -> 10, 2 -> 20))
          Map("1" -> "10", "2" -> "20")
            .transformIntoF[E, Vector[(Int, String)]] ==>
            Right(Vector(1 -> "10", 2 -> "20"))
          Array("1" -> "10", "2" -> "20").transformIntoF[E, Map[Int, Int]] ==>
            Right(Map(1 -> 10, 2 -> 20))
          Array("1" -> "10", "2" -> "20")
            .transformIntoF[E, Map[String, Int]] ==>
            Right(Map("1" -> 10, "2" -> 20))
          Map("1" -> "10", "2" -> "20")
            .transformIntoF[E, Array[(Int, Int)]]
            .toOption
            .get ==>
            Array(1 -> 10, 2 -> 20)
          Map("1" -> "10", "2" -> "20")
            .transformIntoF[E, Array[(Int, String)]]
            .toOption
            .get ==>
            Array(1 -> "10", 2 -> "20")

          Map("1" -> "x", "y" -> "20").transformIntoF[E, Map[Int, Int]] ==>
            Left(List("bad int"))
          Map("x" -> "10", "2" -> "20").transformIntoF[E, Map[Int, String]] ==>
            Left(List("bad int"))
          Seq("1" -> "10", "2" -> "x").transformIntoF[E, Map[Int, Int]] ==>
            Left(List("bad int"))
          ArrayBuffer("1" -> "x", "2" -> "y")
            .transformIntoF[E, Map[String, Int]] ==>
            Left(List("bad int"))
          Map("x" -> "10", "y" -> "z")
            .transformIntoF[E, ArrayBuffer[(Int, Int)]] ==>
            Left(List("bad int"))
          Map("1" -> "10", "x" -> "20")
            .transformIntoF[E, Vector[(Int, String)]] ==>
            Left(List("bad int"))
          Array("x" -> "y", "z" -> "v").transformIntoF[E, Map[Int, Int]] ==>
            Left(List("bad int"))
          Array("1" -> "x", "2" -> "y").transformIntoF[E, Map[String, Int]] ==>
            Left(List("bad int"))
          Map("1" -> "10", "x" -> "20").transformIntoF[E, Array[(Int, Int)]] ==>
            Left(List("bad int"))
          Map("x" -> "10", "y" -> "20")
            .transformIntoF[E, Array[(Int, String)]] ==>
            Left(List("bad int"))
        }
      }
    }

    "wrapped eithers" - {

      type E = [t] =>> Either[List[String], t]

      "pure inner transformer" - {

        given intPrinter: Transformer[Int, String] = _.toString

        "F = Option" - {

          (Left(1): Either[Int, Int])
            .transformIntoF[Option, Either[String, String]] ==> Some(Left("1"))
          (Right(1): Either[Int, Int])
            .transformIntoF[Option, Either[String, String]] ==> Some(Right("1"))
          // Left(1).transformIntoF[Option, Either[String, String]] ==> Some(Left("1")) naked left/right not supported
          // Right(1).transformIntoF[Option, Either[String, String]] ==> Some(Right("1"))
          // Left(1).transformIntoF[Option, Left[String, String]] ==> Some(Left("1"))
          // Right(1).transformIntoF[Option, Right[String, String]] ==> Some(Right("1"))
        }

        "F = Either[List[String], +*]]" - {

          (Left(1): Either[Int, Int])
            .transformIntoF[E, Either[String, String]] ==>
            Right(Left("1"))
          (Right(1): Either[Int, Int])
            .transformIntoF[E, Either[String, String]] ==>
            Right(Right("1"))
          // Left(1).transformIntoF[E, Either[String, String]] ==> Right(Left("1"))
          // Right(1).transformIntoF[E, Either[String, String]] ==> Right(Right("1"))
          // Left(1).transformIntoF[E, Left[String, String]] ==> Right(Left("1"))
          // Right(1).transformIntoF[E, Right[String, String]] ==> Right(Right("1"))
        }
      }

      "wrapped inner transformer" - {

        "F = Option" - {
          given intParserOpt: TransformerF[Option, String, Int] = _.parseInt

          (Left("1"): Either[String, String])
            .transformIntoF[Option, Either[Int, Int]] ==> Some(Left(1))
          (Right("1"): Either[String, String])
            .transformIntoF[Option, Either[Int, Int]] ==> Some(Right(1))
          // Left("1").transformIntoF[Option, Either[Int, Int]] ==> Some(Left(1))
          // Right("1").transformIntoF[Option, Either[Int, Int]] ==> Some(Right(1))
          // Left("1").transformIntoF[Option, Left[Int, Int]] ==> Some(Left(1))
          // Right("1").transformIntoF[Option, Right[Int, Int]] ==> Some(Right(1))

          (Left("x"): Either[String, String])
            .transformIntoF[Option, Either[Int, Int]] ==> None
          (Right("x"): Either[String, String])
            .transformIntoF[Option, Either[Int, Int]] ==> None
          // Left("x").transformIntoF[Option, Either[Int, Int]] ==> None
          // Right("x").transformIntoF[Option, Either[Int, Int]] ==> None
          // Left("x").transformIntoF[Option, Left[Int, Int]] ==> None
          // Right("x").transformIntoF[Option, Right[Int, Int]] ==> None
        }

        "F = Either[List[String], +*]]" - {
          given intParserEither: TransformerF[E, String, Int] =
            _.parseInt.toEither("bad int")

          (Left("1"): Either[String, String])
            .transformIntoF[E, Either[Int, Int]] ==>
            Right(Left(1))
          (Right("1"): Either[String, String])
            .transformIntoF[E, Either[Int, Int]] ==>
            Right(Right(1))
          // Left("1").transformIntoF[E, Either[Int, Int]] ==> Right(Left(1))
          // Right("1").transformIntoF[E, Either[Int, Int]] ==> Right(Right(1))
          // Left("1").transformIntoF[E, Either[Int, Int]] ==> Right(Left(1))
          // Right("1").transformIntoF[E, Either[Int, Int]] ==> Right(Right(1))

          (Left("x"): Either[String, String])
            .transformIntoF[E, Either[Int, Int]] ==>
            Left(List("bad int"))
          (Right("x"): Either[String, String])
            .transformIntoF[E, Either[Int, Int]] ==>
            Left(List("bad int"))
          // Left("x").transformIntoF[E, Either[Int, Int]] ==> Left(List("bad int"))
          // Right("x").transformIntoF[E, Either[Int, Int]] ==> Left(List("bad int"))
          // Left("x").transformIntoF[E, Either[Int, Int]] ==> Left(List("bad int"))
          // Right("x").transformIntoF[E, Either[Int, Int]] ==> Left(List("bad int"))
        }
      }

      "mixed inner transformer" - {

        "F = Option" - {

          given intPrinter: Transformer[Int, String] = _.toString
          given intParserOpt: TransformerF[Option, String, Int] = _.parseInt

          (Left("1"): Either[String, Int])
            .transformIntoF[Option, Either[Int, String]] ==> Some(Left(1))
          (Left("x"): Either[String, Int])
            .transformIntoF[Option, Either[Int, String]] ==> None
          (Right(100): Either[String, Int])
            .transformIntoF[Option, Either[Int, String]] ==> Some(Right("100"))

          (Left(100): Either[Int, String])
            .transformIntoF[Option, Either[String, Int]] ==> Some(Left("100"))
          (Right("1"): Either[Int, String])
            .transformIntoF[Option, Either[String, Int]] ==> Some(Right(1))
          (Right("x"): Either[Int, String])
            .transformIntoF[Option, Either[String, Int]] ==> None
        }

        "F = Either[List[String], +*]]" - {

          given intPrinter: Transformer[Int, String] = _.toString
          given intParserEither: TransformerF[E, String, Int] =
            _.parseInt.toEither("bad int")

          (Left("1"): Either[String, Int])
            .transformIntoF[E, Either[Int, String]] ==>
            Right(Left(1))
          (Left("x"): Either[String, Int])
            .transformIntoF[E, Either[Int, String]] ==>
            Left(List("bad int"))
          (Right(100): Either[String, Int])
            .transformIntoF[E, Either[Int, String]] ==>
            Right(Right("100"))

          (Left(100): Either[Int, String])
            .transformIntoF[E, Either[String, Int]] ==>
            Right(Left("100"))
          (Right("1"): Either[Int, String])
            .transformIntoF[E, Either[String, Int]] ==>
            Right(Right(1))
          (Right("x"): Either[Int, String])
            .transformIntoF[E, Either[String, Int]] ==>
            Left(List("bad int"))
        }
      }
    }

    "wrapped sealed families" - {
      import examples.*
      type E = [t] =>> Either[List[String], t]

      "pure inner transformer" - {

        given intPrinter: Transformer[Int, String] = _.toString

        "F = Option" - {
          import ScalesTransformer.given

          (short.Zero: short.NumScale[Int, Nothing])
            .transformIntoF[Option, long.NumScale[String]] ==> Some(long.Zero)
          (short.Million(4): short.NumScale[Int, Nothing])
            .transformIntoF[Option, long.NumScale[String]] ==> Some(
            long.Million("4")
          )
          (short.Billion(2): short.NumScale[Int, Nothing])
            .transformIntoF[Option, long.NumScale[String]] ==> Some(
            long.Milliard("2")
          )
          (short.Trillion(100): short.NumScale[Int, Nothing])
            .transformIntoF[Option, long.NumScale[String]] ==> Some(
            long.Billion("100")
          )
        }

        "F = Either[List[String], +*]]" - {
          import ScalesTransformer.given

          (short.Zero: short.NumScale[Int, Nothing])
            .transformIntoF[E, long.NumScale[String]] ==> Right(long.Zero)
          (short.Million(4): short.NumScale[Int, Nothing])
            .transformIntoF[E, long.NumScale[String]] ==> Right(
            long.Million("4")
          )
          (short.Billion(2): short.NumScale[Int, Nothing])
            .transformIntoF[E, long.NumScale[String]] ==> Right(
            long.Milliard("2")
          )
          (short.Trillion(100): short.NumScale[Int, Nothing])
            .transformIntoF[E, long.NumScale[String]] ==> Right(
            long.Billion("100")
          )
        }
      }

      "wrapped inner transformer" - {

        "F = Option" - {
          given intParserOpt: TransformerF[Option, String, Int] = _.parseInt

          import ScalesTransformer.given

          (short.Zero: short.NumScale[String, Nothing])
            .transformIntoF[Option, long.NumScale[Int]] ==> Some(long.Zero)
          (short.Million("4"): short.NumScale[String, Nothing])
            .transformIntoF[Option, long.NumScale[Int]] ==> Some(
            long.Million(4)
          )
          (short.Billion("2"): short.NumScale[String, Nothing])
            .transformIntoF[Option, long.NumScale[Int]] ==> Some(
            long.Milliard(2)
          )
          (short.Trillion("100"): short.NumScale[String, Nothing])
            .transformIntoF[Option, long.NumScale[Int]] ==> Some(
            long.Billion(100)
          )

          (short.Million("x"): short.NumScale[String, Nothing])
            .transformIntoF[Option, long.NumScale[Int]] ==> None
          (short.Billion("x"): short.NumScale[String, Nothing])
            .transformIntoF[Option, long.NumScale[Int]] ==> None
          (short.Trillion("x"): short.NumScale[String, Nothing])
            .transformIntoF[Option, long.NumScale[Int]] ==> None
        }

        "F = Either[List[String], +*]]" - {
          given intParserEither: TransformerF[E, String, Int] =
            _.parseInt.toEither("bad int")

          import ScalesTransformer.given

          (short.Zero: short.NumScale[String, Nothing])
            .transformIntoF[E, long.NumScale[Int]] ==> Right(long.Zero)
          (short.Million("4"): short.NumScale[String, Nothing])
            .transformIntoF[E, long.NumScale[Int]] ==> Right(long.Million(4))
          (short.Billion("2"): short.NumScale[String, Nothing])
            .transformIntoF[E, long.NumScale[Int]] ==> Right(long.Milliard(2))
          (short.Trillion("100"): short.NumScale[String, Nothing])
            .transformIntoF[E, long.NumScale[Int]] ==> Right(long.Billion(100))

          (short.Million("x"): short.NumScale[String, Nothing])
            .transformIntoF[E, long.NumScale[Int]] ==> Left(List("bad int"))
          (short.Billion("x"): short.NumScale[String, Nothing])
            .transformIntoF[E, long.NumScale[Int]] ==> Left(List("bad int"))
          (short.Trillion("x"): short.NumScale[String, Nothing])
            .transformIntoF[E, long.NumScale[Int]] ==> Left(List("bad int"))
        }
      }
    }

    "support conversion from java beans" - {
      import io.scalaland.chimney.example.*

      val bean = new JavaBean
      bean.setId(42)
      bean.setName(null)

      "converting directly with nulls" - {

        bean.intoF[Option, ScalaBean].enableBeanGetters.transform ==> Some(ScalaBean(42, null))
        bean
          .intoF[[t] =>> Either[String, t], ScalaBean]
          .enableBeanGetters
          .transform ==> Right(ScalaBean(42, null))
      }

      "converting nulls to Option.None if possible" - {
        bean.intoF[Option, ScalaBeanOpt].enableBeanGetters.transform ==> Some(
          ScalaBeanOpt(Some(42), None)
        )
      }

      val javaInput = new java.util.ArrayList[Int]()
      javaInput.add(1)
      javaInput.add(11)

      "converting nested java beans to scala classes" - {
        import scala.jdk.CollectionConverters._
        val nestingBean = new NestedBean()
        val bean = new JavaBean
        bean.setId(42)
        bean.setName(null)

        nestingBean.setIds(javaInput.asInstanceOf)
        nestingBean.setNested(bean)

        nestingBean
          .intoF[Option, NestedScalaBean]
          .enableBeanGetters
          .withFieldComputed(_.ids, _.getIds.asScala.toList)
          .transform ==>
          Some(
            NestedScalaBean(
              List(1, 11),
              ScalaBeanOpt(Some(42), None)
            )
          )

      }
    }
  }

}
