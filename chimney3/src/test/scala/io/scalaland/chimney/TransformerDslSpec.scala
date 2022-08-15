package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.examples.*
import io.scalaland.chimney.internal.utils.MacroUtils
import io.scalaland.chimney.internal.TransformerFlag
import utest.*

object TransformerDslSpec extends TestSuite {

  val tests = Tests {

    "use given instances directly" - {
      import Domain1.*

      given instance: Transformer[UserName, String] =
        userNameToStringTransformer

      UserName("Batman").into[String].transform ==> "BatmanT"
      UserName("Batman").transformInto[String] ==> "BatmanT"
    }

    "use given transformer for nested field" - {
      import Domain1.*

      given instance: Transformer[UserName, String] =
        userNameToStringTransformer

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

        "not compile if source for the target fields is not provided" - {

          compileError("Bar(3, (3.14, 3.14)).transformInto[Foo]")
            .check(
              "",
              "Unable to find default value in Foo or method in Bar for \"y\" at"
            )
        }

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

          "not compile if .enableOptionDefaultsToNone is missing" - {
            compileError(
              """SomeFoo("foo").into[Foobar].transform ==> Foobar("foo", None)"""
            )
              .check(
                "",
                "Unable to find default value in io.scalaland.chimney.TransformerDslSpec.Foobar or method in io.scalaland.chimney.TransformerDslSpec.SomeFoo for \"y\" at"
              )
          }

          "target has default value, but default values are disabled and .enableOptionDefaultsToNone" - {
            fooToFoobar2OptDefNone ==> Foobar2("foo", None)
          }

          "not use None as default when other default value is set" - {
            fooToFoobar2NoOptDef ==> Foobar2("foo", Some(42))
            fooToFoobar2PrederDefault ==> Foobar2("foo", Some(42))
          }

          "not compile if default value is missing and no .enableOptionDefaultsToNone" - {
            compileError("""SomeFoo("foo").into[Foobar].transform""")
              .check(
                "",
                "Unable to find default value in io.scalaland.chimney.TransformerDslSpec.Foobar or method in io.scalaland.chimney.TransformerDslSpec.SomeFoo for \"y\" at "
              )
          }

          "not compile if default values are disabled and no .enableOptionDefaultsToNone" - {
            compileError(
              """SomeFoo("foo").into[Foobar2].disableDefaultValues.transform"""
            )
              .check(
                "",
                "Unable to find default value in io.scalaland.chimney.TransformerDslSpec.Foobar2 or method in io.scalaland.chimney.TransformerDslSpec.SomeFoo for \"y\" at "
              )
          }
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

      "not compile when default parameter values are disabled" - {
        import DefaultSupportSpecs.*
        compileError("""
          Foo(10).into[Bar].disableDefaultValues.transform
        """)
          .check(
            "",
            "Unable to find default value in io.scalaland.chimney.TransformerDslSpec.DefaultSupportSpecs.Bar or method in io.scalaland.chimney.TransformerDslSpec.DefaultSupportSpecs.Foo for \"y\" at "
          )

        compileError("""
          Baah(10, Foo(300)).into[Baahr].disableDefaultValues.transform
        """)
          .check(
            "",
            "Unable to find default value in io.scalaland.chimney.TransformerDslSpec.DefaultSupportSpecs.Bar or method in io.scalaland.chimney.TransformerDslSpec.DefaultSupportSpecs.Foo for \"y\" at "
          )
      }
    }

    "transform with rename" - {

      "between different types: correct" - {
        TransformWithRenameSpecs.`between different types: correct`
      }

      "between different types: incorrect" - {
        TransformWithRenameSpecs.`between different types: incorrect`
      }

      "between different types: without implicit" - {
        compileError("""
            import TransformWithRenameSpecs.*
            val user: User = User(1, "Kuba", None)
            user.into[UserPL].withFieldRenamed(_.name, _.imie)
                .withFieldRenamed(_.age, _.wiek)
                .transform
          """)
          .check(
            "",
            "Unable to find default value in scala.util.Left[scala.Unit, scala.Int] or method in scala.None.type for \"value\" at .age.None$"
          )
      }
    }

    "support relabelling of fields" - {

      "not compile if relabelling modifier is not provided" - {
        import RelabelingOfFieldSpec.*

        compileError("""Foo(10, "something").transformInto[Bar]""")
          .check(
            "",
            "Unable to find default value in io.scalaland.chimney.TransformerDslSpec.RelabelingOfFieldSpec.Bar or method in io.scalaland.chimney.TransformerDslSpec.RelabelingOfFieldSpec.Foo for \"z\" at "
          )
      }

      "relabel fields with relabelling modifier" - {
        RelabelingOfFieldSpec.`relabel fields with relabelling modifier`
      }

      "not compile if relabelling selectors are invalid" - {
        import RelabelingOfFieldSpec.*

        compileError("""
            Foo(10, "something")
              .into[Bar]
              .withFieldRenamed(_.y + "abc", _.z)
              .transform
          """)
          .check(
            "",
            """Illegal selector: ((x: io.scalaland.chimney.TransformerDslSpec.RelabelingOfFieldSpec.Foo) => x.y.+("abc"))"""
          )

        compileError("""
            val haveY = HaveY("")
            Foo(10, "something")
              .into[Bar]
              .withFieldRenamed(cc => haveY.y, _.z)
              .transform
          """)
          .check(
            "",
            """Illegal selector: ((cc: io.scalaland.chimney.TransformerDslSpec.RelabelingOfFieldSpec.Foo) => haveY.y)"""
          )

        compileError("""
            Foo(10, "something")
              .into[Bar]
              .withFieldRenamed(_.y, _.z + "abc")
              .transform
          """)
          .check(
            "",
            """Illegal selector: ((x: io.scalaland.chimney.TransformerDslSpec.RelabelingOfFieldSpec.Bar) => x.z.+("abc")"""
          )

        compileError("""
            val haveZ = HaveZ("")
            Foo(10, "something")
              .into[Bar]
              .withFieldRenamed(_.y, cc => haveZ.z)
              .transform
          """)
          .check(
            "",
            """Illegal selector: ((cc: io.scalaland.chimney.TransformerDslSpec.RelabelingOfFieldSpec.Bar) => haveZ.z)"""
          )

        compileError("""
            Foo(10, "something")
              .into[Bar]
              .withFieldRenamed(_.y + "abc", _.z + "abc")
              .transform
          """)
          .check(
            "",
            """Illegal selector: ((x: io.scalaland.chimney.TransformerDslSpec.RelabelingOfFieldSpec.Foo) => x.y.+("abc"))"""
          )

        compileError("""
            val haveY = HaveY("")
            val haveZ = HaveZ("")
            Foo(10, "something")
              .into[Bar]
              .withFieldRenamed(cc => haveY.y, cc => haveZ.z)
              .transform
          """)
          .check(
            "",
            """Illegal selector: ((cc: io.scalaland.chimney.TransformerDslSpec.RelabelingOfFieldSpec.Foo) => haveY.y)"""
          )
      }

      "not compile if relabelled - a wrong way" - {
        import RelabelingOfFieldSpec.*

        compileError(
          """Foo(10, "something").into[Bar].withFieldRenamed(_.y, _.x).transform"""
        )
          .check(
            "",
            "Automatic derivation for supplied types (from java.lang.String to scala.Int) not supported at .y"
          )

        compileError(
          """Foo(10, "something").into[Bar].withFieldRenamed(_.x, _.z).transform"""
        )
          .check(
            "",
            "Automatic derivation for supplied types (from scala.Int to java.lang.String) not supported at .x"
          )
      }

    }

    "support common data-types" - {
      import CommonDataTypesSpec.*

      "support scala.Option" - {
        Option(Foo("a")).transformInto[Option[Bar]] ==> Option(Bar("a"))
        (Some(Foo("a")): Option[Foo]).transformInto[Option[Bar]] ==> Option(
          Bar("a")
        )
        Some(Foo("a")).transformInto[Option[Bar]] ==> Some(Bar("a"))
        (None: Option[Foo]).transformInto[Option[Bar]] ==> None
        (None: Option[String]).transformInto[Option[String]] ==> None
        Option("abc").transformInto[Option[String]] ==> Some("abc")
      }

      "support scala.util.Either" - {
        (Left(Foo("a")): Either[Foo, Foo])
          .transformInto[Either[Bar, Bar]] ==> Left(Bar("a"))
        (Right(Foo("a")): Either[Foo, Foo])
          .transformInto[Either[Bar, Bar]] ==> Right(Bar("a"))
        // Left(Foo("a")).transformInto[Either[Bar, Bar]] ==> Left(Bar("a"))
        // Right(Foo("a")).transformInto[Either[Bar, Bar]] ==> Right(Bar("a"))
        // Left(Foo("a")).transformInto[Left[Bar, Bar]] ==> Left(Bar("a"))
        // Right(Foo("a")).transformInto[Right[Bar, Bar]] ==> Right(Bar("a"))
        (Left("a"): Either[String, String])
          .transformInto[Either[String, String]] ==> Left("a")
        (Right("a"): Either[String, String])
          .transformInto[Either[String, String]] ==> Right("a")
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
        Seq(Bar("x"), Bar("y"))
          .transformInto[Array[Foo]] ==> Array(Foo("x"), Foo("y"))
      }

      "support Map" - {
        Map("test" -> Foo("a")).transformInto[Map[String, Bar]] ==> Map(
          "test" -> Bar("a")
        )
        Map("test" -> "a").transformInto[Map[String, String]] ==> Map(
          "test" -> "a"
        )
        Map(Foo("test") -> "x").transformInto[Map[Bar, String]] ==> Map(
          Bar("test") -> "x"
        )
        Map(Foo("test") -> Foo("x")).transformInto[Map[Bar, Bar]] ==> Map(
          Bar("test") -> Bar("x")
        )
      }

      "support conversion between Iterables and Maps" - {

        Seq(Foo("10") -> Bar("20"), Foo("20") -> Bar("40"))
          .transformInto[Map[Bar, Foo]] ==>
          Map(Bar("10") -> Foo("20"), Bar("20") -> Foo("40"))

        Map(Foo("10") -> Bar("20"), Foo("20") -> Bar("40"))
          .transformInto[List[(Bar, Foo)]] ==>
          List(Bar("10") -> Foo("20"), Bar("20") -> Foo("40"))
      }

      "support conversion between Arrays and Maps" - {

        Array(Foo("10") -> Bar("20"), Foo("20") -> Bar("40"))
          .transformInto[Map[Bar, Foo]] ==>
          Map(Bar("10") -> Foo("20"), Bar("20") -> Foo("40"))

        Map(Foo("10") -> Bar("20"), Foo("20") -> Bar("40"))
          .transformInto[Array[(Bar, Foo)]] ==>
          Array(Bar("10") -> Foo("20"), Bar("20") -> Foo("40"))
      }

    }

    "support sealed hierarchies" - {

      "enum types encoded as sealed hierarchies of case objects" - {
        "transforming from smaller to bigger enum" - {

          inline given transformer: Transformer[colors1.Color, colors2.Color] =
            Transformer.derived[colors1.Color, colors2.Color]

          (colors1.Red: colors1.Color)
            .transformInto[colors2.Color] ==> colors2.Red
          (colors1.Green: colors1.Color)
            .transformInto[colors2.Color] ==> colors2.Green
          (colors1.Blue: colors1.Color)
            .transformInto[colors2.Color] ==> colors2.Blue
        }

        "transforming from bigger to smaller enum" - {

          def blackIsRed(b: colors2.Black.type): colors1.Color =
            colors1.Red

          inline given transformer: Transformer[colors2.Color, colors1.Color] =
            defaultDefinition[colors2.Color, colors1.Color]
              .withCoproductInstance(blackIsRed)
              .buildTransformer

          (colors2.Black: colors2.Color)
            .transformInto[colors1.Color] ==> colors1.Red

          (colors2.Red: colors2.Color)
            .transformInto[colors1.Color] ==> colors1.Red

          (colors2.Green: colors2.Color)
            .transformInto[colors1.Color] ==> colors1.Green

          (colors2.Blue: colors2.Color)
            .transformInto[colors1.Color] ==> colors1.Blue
        }

      }

      "transforming non-isomorphic domains" - {

        def triangleToPolygon(t: shapes1.Triangle): shapes2.Shape =
          shapes2.Polygon(
            List(
              t.p1.transformInto[shapes2.Point],
              t.p2.transformInto[shapes2.Point],
              t.p3.transformInto[shapes2.Point]
            )
          )

        def rectangleToPolygon(r: shapes1.Rectangle): shapes2.Shape =
          shapes2.Polygon(
            List(
              r.p1.transformInto[shapes2.Point],
              shapes2.Point(r.p1.x, r.p2.y),
              r.p2.transformInto[shapes2.Point],
              shapes2.Point(r.p2.x, r.p1.y)
            )
          )

        val triangle: shapes1.Shape =
          shapes1.Triangle(
            shapes1.Point(0, 0),
            shapes1.Point(2, 2),
            shapes1.Point(2, 0)
          )

        triangle
          .into[shapes2.Shape]
          .withCoproductInstance(triangleToPolygon)
          .withCoproductInstance(rectangleToPolygon)
          .transform ==> shapes2.Polygon(
          List(shapes2.Point(0, 0), shapes2.Point(2, 2), shapes2.Point(2, 0))
        )
      }

      "transforming isomorphic domains that differ a detail" - {

        given intToDoubleTransformer: Transformer[Int, Double] =
          (_: Int).toDouble

        (shapes1
          .Triangle(
            shapes1.Point(0, 0),
            shapes1.Point(2, 2),
            shapes1.Point(2, 0)
          ): shapes1.Shape)
          .transformInto[shapes3.Shape] ==>
          shapes3.Triangle(
            shapes3.Point(2.0, 0.0),
            shapes3.Point(2.0, 2.0),
            shapes3.Point(0.0, 0.0)
          )

        (shapes1
          .Rectangle(shapes1.Point(0, 0), shapes1.Point(6, 4)): shapes1.Shape)
          .transformInto[shapes3.Shape] ==>
          shapes3.Rectangle(shapes3.Point(0.0, 0.0), shapes3.Point(6.0, 4.0))
      }
    }

    "support polymorphic source/target objects and modifiers" - {

      import Poly.*

      "monomorphic source to polymorphic target" - {

        monoSource.transformInto[PolyTarget[String]] ==> polyTarget

        def transform[T]: (String => T) => MonoSource => PolyTarget[T] =
          fun =>
            _.into[PolyTarget[T]]
              .withFieldComputed(_.poly, src => fun(src.poly))
              .transform

        transform[String](identity)(monoSource) ==> polyTarget
      }

      "polymorphic source to monomorphic target" - {

        def transform[T]: PolySource[T] => MonoTarget =
          _.into[MonoTarget]
            .withFieldComputed(_.poly, _.poly.toString)
            .transform

        transform[String](polySource) ==> monoTarget
      }

      "polymorphic source to polymorphic target" - {

        def transform[T]: PolySource[T] => PolyTarget[T] =
          _.transformInto[PolyTarget[T]]

        transform[String](polySource) ==> polyTarget
      }

      "handle type-inference for polymorphic computation" - {

        def fun[T]: PolySource[T] => String = _.poly.toString

        def transform[T]: PolySource[T] => MonoTarget =
          _.into[MonoTarget].withFieldComputed(_.poly, fun).transform

        transform[String](polySource) ==> monoTarget
      }

    }

    "support abstracting over a value in dsl operations" - {

      case class Foo(x: String)
      case class Bar(z: Double, y: Int, x: String)

      val partialTransformer = Foo("abc")
        .into[Bar]
        .withFieldComputed(_.y, _.x.length)

      val transformer1 = partialTransformer.withFieldConst(_.z, 1.0)
      val transformer2 =
        partialTransformer.withFieldComputed(_.z, _.x.length * 2.0)

      transformer1.transform ==> Bar(1.0, 3, "abc")
      transformer2.transform ==> Bar(6.0, 3, "abc")
    }

    "transform T to Option[T]" - {

      "abc".transformInto[Option[String]] ==> Some("abc")
      (null: String).transformInto[Option[String]] ==> None
    }

    "support recursive data structures" - {
      import RecursiveTypesSpec.*

      "defined by hand" - {
        given fooToBarTransformer: Transformer[Foo, Bar] = (foo: Foo) => {
          Bar(foo.x.map(fooToBarTransformer.transform))
        }

        Foo(Some(Foo(None))).transformInto[Bar] ==> Bar(Some(Bar(None)))
      }

      "generated automatically" - {
        given fooToBarTransformer: Transformer[Foo, Bar] =
          Transformer.derived[Foo, Bar]

        Foo(Some(Foo(None))).transformInto[Bar] ==> Bar(Some(Bar(None)))
      }

      "support mutual recursion" - {

        given bar1ToBar2Transformer: Transformer[Bar1, Bar2] =
          Transformer.derived[Bar1, Bar2]

        Bar1(1, Baz(Some(Bar1(2, Baz(None))))).transformInto[Bar2] ==> Bar2(
          Baz(Some(Bar2(Baz(None))))
        )
      }
    }

    "support conversion from java beans" - {
      import io.scalaland.chimney.example.*

      val bean = new JavaBean
      bean.setId(42)
      bean.setName(null)

      "converting directly with nulls" - {
        bean.into[ScalaBean].enableBeanGetters.transform ==> ScalaBean(42, null)
      }

      "converting nulls to Option.None if possible" - {
        bean.into[ScalaBeanOpt].enableBeanGetters.transform ==> ScalaBeanOpt(
          Some(42),
          None
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
          .into[NestedScalaBean]
          .enableBeanGetters
          .withFieldComputed(_.ids, _.getIds.asScala.toList)
          .transform ==> NestedScalaBean(
          List(1, 11),
          ScalaBeanOpt(Some(42), None)
        )

      }
    }

//    "suppot conversion to java beans" - {
//      import io.scalaland.chimney.example.JavaBean
//
//      "converting directly" - {
//        val bean = ScalaBean(42, "testing")
//        val result = bean.into[JavaBean].enableBeanSetters.transform
//        result.getId ==> 42
//        result.getName ==> "testing"
//      }
//    }

  }

//workaround scala3 bugs, hopefully will be fixed one day
  case class SomeFoo(x: String)
  case class Foobar(x: String, y: Option[Int])
  case class Foobar2(x: String, y: Option[Int] = Some(42))

  lazy val fooToFoobarOptDefault =
    SomeFoo("foo").into[Foobar].enableOptionDefaultsToNone.transform
  lazy val fooToFoobar2OptDefNone = SomeFoo("foo")
    .into[Foobar2]
    .disableDefaultValues
    .enableOptionDefaultsToNone
    .transform
  lazy val fooToFoobar2NoOptDef = SomeFoo("foo").into[Foobar2].transform
  lazy val fooToFoobar2PrederDefault =
    SomeFoo("foo").into[Foobar2].enableOptionDefaultsToNone.transform

  object DefaultSupportSpecs {
    case class Foo(x: Int)
    case class Bar(x: Int, y: Long = 30L)
    case class Baz(x: Int = 5, y: Long = 100L)
    case class Baah(x: Int, y: Foo = Foo(0))
    case class Baahr(x: Int, y: Bar)

    def `field does not exists - the source-single` =
      Foo(10).transformInto[Bar] ==> Bar(10, 30L)
    def `field does not exists - the source-sequence` = Seq(Foo(30), Foo(40))
      .transformInto[Seq[Bar]] ==> Seq(Bar(30, 30L), Bar(40, 30L))

    def `field does not exists - nested object` =
      Baah(10, Foo(300)).transformInto[Baahr] ==> Baahr(10, Bar(300, 30L))

    def `field exists - the source-single` =
      Bar(100, 200L).transformInto[Baz] ==> Baz(100, 200L)
    def `field exists - the source-sequence` =
      Seq(Bar(100, 200L), Bar(300, 400L))
        .transformInto[Seq[Baz]] ==> Seq(Baz(100, 200L), Baz(300, 400L))

    def `another modifier is provided` =
      Foo(10).into[Bar].withFieldConst(_.y, 45L).transform ==> Bar(10, 45L)

    def `default values are disabled and another modifier is provided-one` =
      Foo(10)
        .into[Bar]
        .disableDefaultValues
        .withFieldConst(_.y, 45L)
        .transform ==> Bar(10, 45L)

    def `default values are disabled and another modifier is provided-two` =
      Foo(10)
        .into[Bar]
        .withFieldConst(_.y, 48L)
        .disableDefaultValues
        .transform ==> Bar(10, 48L)

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
      given trans: Transformer[Option[Int], Either[Unit, Int]] =
        ageToWiekTransformer

      val user: User = User(1, "Kuba", Some(28))
      val userPl = UserPL(1, "Kuba", Right(28))
      user
        .into[UserPL]
        .withFieldRenamed(_.name, _.imie)
        .withFieldRenamed(_.age, _.wiek)
        .transform ==> userPl

    def `between different types: incorrect` =
      given trans: Transformer[Option[Int], Either[Unit, Int]] =
        ageToWiekTransformer

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

    def `relabel fields with relabelling modifier` =
      Foo(10, "something")
        .into[Bar]
        .withFieldRenamed(_.y, _.z)
        .transform ==> Bar(10, "something")

  }

  object CommonDataTypesSpec {
    case class Foo(value: String)
    case class Bar(value: String)
  }

  object RecursiveTypesSpec {

    case class Foo(x: Option[Foo])
    case class Bar(x: Option[Bar])

    case class Baz[T](bar: Option[T])
    case class Bar1(x: Int, foo: Baz[Bar1])
    case class Bar2(foo: Baz[Bar2])
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

case class ScalaBean(
  id: Int,
  name: String
)

case class ScalaBeanOpt(
  id: Option[Int],
  name: Option[String]
)

case class NestedScalaBean(
  ids: List[Int],
  nested: ScalaBeanOpt
)
