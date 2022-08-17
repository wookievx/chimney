package io.scalaland.chimney.internal.utils

import scala.quoted.{*, given}
import deriving.*
import compiletime.*

object MacroPlayground:

  inline def selectValue(value: Mock): Unit = ${ selectValueImpl('value) }

  inline def selectValueInline(inline value: Mock): Unit = ${
    selectValueImpl('value)
  }

  def selectValueImpl(value: Expr[Mock])(using q: Quotes): Expr[Unit] =
    import q.reflect._
    println('{ $value.field }.asTerm)
    '{}
  end selectValueImpl

  case class Mock(field: String, specialField: Int):
    def theField: String = field
    def theSpecialField: Int = specialField

  def d2a =
    import io.scalaland.chimney.dsl.*

    class JavaBean {
      def getId(): Int = 42
      def getName(): String = null
    }
    case class ScalaBean(id: Int, name: String)

    val bean = new JavaBean

    bean.into[ScalaBean].enableBeanGetters.transform

  end d2a

end MacroPlayground
