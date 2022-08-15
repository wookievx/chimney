package io.scalaland.chimney

import utest.*
import io.scalaland.chimney.internal.utils.ClassAccessMacros
import io.scalaland.chimney.internal.utils.MacroUtils

object ClassAccessMacrosSpec extends TestSuite {

  def tests: Tests = Tests {
    import io.scalaland.chimney.example.JavaBean

    "ClassAccessMacros" - {
//      "getEmptyInstance should create correct instances of classes" - {
//        val testObj = ClassAccessMacros.getEmptyInstance[JavaBean]
//        testObj.isDefined ==> true
//        val obj = testObj.get
//        obj.setId(42)
//        obj.setName("test")
//        obj.getId ==> 42
//        obj.getName ==> "test"
//      }
    }
  }

  case class ScalaBean(
    id: Int,
    name: String
  )

  case class ScalaBeanOpt(
    id: Option[Int],
    name: Option[String]
  )

}
