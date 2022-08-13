package io.scalaland.chimney.internal.utils

import scala.quoted.{given, *}
import deriving.*, compiletime.*

object ClassAccessMacros:

  transparent inline def selectByName[C](
    cValue: C,
    inline name: Any
  ) = ${
    selectByNameAny('cValue, 'name)
  }

  transparent inline def selectBeanGetter[C](
    cValue: C,
    inline name: Any
  ) = ${
    selectBeanImpl('cValue, 'name)
  }

  inline def selectByName1[C, Arg](
    cValue: C,
    inline name: Any,
    arg: Arg
  ): Any = ${ select1ByNameAny('cValue, 'name, 'arg) }

  inline def selectBeanSetter[C, Arg](
    cValue: C,
    inline name: Any,
    arg: Arg
  ): Any = ${ selectBean1Impl('cValue, 'name, 'arg) }

  private def selectBeanImpl[C: Type](
    cValue: Expr[C],
    name: Expr[Any]
  )(using
    Quotes
  ): Expr[Any] =
    import quotes.reflect.report
    name match
      case '{ $n: String } =>
        n.value match {
          case Some(name) =>
            val getterName = s"get${name.updated(0, name(0).toUpper)}"
            selectByNameImpl(cValue, getterName) match
              case Some('{ $expr: t }) => '{ Some($expr) }
              case _                   => '{ None }
          case None =>
            report.errorAndAbort(
              "Failed to extract name, not a constant for some reason"
            )
        }

      case _ =>
        report.errorAndAbort(
          "Failed to extract name it is not a string for some reason"
        )
  end selectBeanImpl

  private def selectBean1Impl[C: Type, Arg: Type](
    cValue: Expr[C],
    name: Expr[Any],
    arg: Expr[Arg]
  )(using
    Quotes
  ): Expr[Any] =
    import quotes.reflect.report
    name match
      case '{ $n: String } =>
        n.value match {
          case Some(name) =>
            val getterName = s"set${name.updated(0, name(0).toUpper)}"
            select1ByNameImpl(cValue, getterName, arg) match
              case Some(expr) => expr
              case _          => '{}
          case None =>
            report.errorAndAbort(
              "Failed to extract name, not a constant for some reason"
            )
        }

      case _ =>
        report.errorAndAbort(
          "Failed to extract name it is not a string for some reason"
        )
  end selectBean1Impl

  private def selectByNameAny[C: Type](
    cValue: Expr[C],
    name: Expr[Any]
  )(using
    Quotes
  ): Expr[Any] =
    import quotes.reflect.report
    name match
      case '{ $n: String } =>
        n.value match {
          case Some(name) =>
            selectByNameImpl(cValue, name) match
              case Some(expr) => '{ Some($expr) }
              case None       => '{ None }
          case None =>
            report.errorAndAbort(
              "Failed to extract name, not a constant for some reason"
            )
        }

      case _ =>
        report.errorAndAbort(
          "Failed to extract name it is not a string for some reason"
        )
  end selectByNameAny

  private def selectByNameImpl[C: Type](
    cValue: Expr[C],
    name: String
  )(using
    Quotes
  ): Option[Expr[Any]] =
    import quotes.reflect.*
    val sym = TypeTree.of[C].symbol
    val methods: List[Symbol] = sym.declaredMethods
    val method = methods.collectFirst { case s @ SimpleAccessor(`name`) => s }

    method
      .flatMap { sym =>
        sym.paramSymss match
          case Nil => //no arguments
            Some(Select.unique(cValue.asTerm, name))
          case List() :: Nil => // empty argument list
            Some(
              Apply(
                Select(cValue.asTerm, sym),
                List.empty
              )
            )
          case signature =>
            None
      }
      .map { term =>
        term.tpe.asType match
          case '[tpe] =>
            term.asExpr.asExprOf[tpe]
      }
  end selectByNameImpl

  private def select1ByNameAny[C: Type, Arg: Type](
    cValue: Expr[C],
    name: Expr[Any],
    arg: Expr[Arg]
  )(using Quotes): Expr[Any] =
    import quotes.reflect.*
    name match
      case '{ $n: String } =>
        n.value match {
          case Some(name) =>
            select1ByNameImpl(cValue, name, arg) match
              case Some(expr) => expr
              case None       => '{}
          case None =>
            report.errorAndAbort(
              "Failed to extract name, not a constant for some reason"
            )
        }

      case _ =>
        report.errorAndAbort(
          "Failed to extract name it is not a string for some reason"
        )
  end select1ByNameAny

  private def select1ByNameImpl[C: Type, Arg: Type](
    cValue: Expr[C],
    name: String,
    arg: Expr[Arg]
  )(using
    Quotes
  ): Option[Expr[Any]] =
    import quotes.reflect.*
    val sym = TypeTree.of[C].symbol
    val methods: List[Symbol] = sym.declaredMethods
    val method = methods.collectFirst { case s @ Setter(`name`) => s }

    method
      .flatMap { sym =>
        Some(
          Apply(
            Select(cValue.asTerm, sym),
            List(arg.asTerm)
          ).asExpr
        )
      }

  end select1ByNameImpl



  private def findMethod[C: Type](name: String)(using
    Quotes
  ): Option[(Type[?], quotes.reflect.Symbol)] =
    import quotes.reflect.*
    val sym = TypeTree.of[C].symbol
    if sym.isClassDef then
      val methods: List[Symbol] = sym.declaredMethods
      val rawMethod = methods.collectFirst {
        case s if s.name == name => s
      }
      rawMethod.map(s => TypeRepr.of[C].memberType(s).asType -> s)
    else None
  end findMethod

  inline def listMethods[C]: List[String] = ${
    listMethodsImpl[C]
  }

  private def listMethodsImpl[C: Type](using
    Quotes
  ): Expr[List[String]] =
    import quotes.reflect.*

    val sym = TypeTree.of[C].symbol

    if sym.isClassDef then Expr[List[String]](sym.declaredMethods.map(_.name))
    else Expr[List[String]](List.empty)
  end listMethodsImpl

  transparent inline def getEmptyInstance[C]: Option[C] = ${
    getEmptyInstanceMacro[C]
  }

  inline def getEmptyInstanceFactory[C]: Option[() => C] = ${
    getEmptyInstanceFactoryMacro[C]
  }

  private def getEmptyInstanceFactoryMacro[C: Type](using
    Quotes
  ): Expr[Option[() => C]] =
    getEmptyInstanceImpl[C] match
      case Some('{ $expr: C }) => '{ Some(() => $expr) }
      case _                   => '{ None }
  end getEmptyInstanceFactoryMacro

  private def getEmptyInstanceMacro[C: Type](using Quotes): Expr[Option[C]] =
    import quotes.reflect.*
    getEmptyInstanceImpl[C] match
      case Some('{ $expr: C }) => '{ Some($expr) }
      case _                   => '{ None }
  end getEmptyInstanceMacro

  private def getEmptyInstanceImpl[C: Type](using Quotes): Option[Expr[C]] =
    import quotes.reflect.*
    val sym = TypeTree.of[C].symbol
    val constructor = sym.primaryConstructor
    if (constructor.exists) then
      Some(
        Apply(
          Select(New(TypeTree.of[C]), constructor),
          List()
        ).asExpr.asExprOf[C]
      )
    else None

  end getEmptyInstanceImpl

  private object SimpleAccessor:
    def unapply(using Quotes)(
      arg: quotes.reflect.Symbol
    ): Option[String] =
      import quotes.reflect.*
      arg.paramSymss match
        case Nil => //no arguments
          Some(arg.name)
        case List() :: Nil => // empty argument list
          Some(arg.name)
        case _ =>
          None
  end SimpleAccessor

  private object Setter:
    def unapply(using Quotes)(
      arg: quotes.reflect.Symbol
    ): Option[String] =
      import quotes.reflect.*

      val valueArguments = arg.signature.paramSigs match
        case (_: String) :: Nil => //simple setter
          Some(arg.paramSymss)
        case _ =>
          None

      valueArguments
        .flatMap {
          case (_ :: Nil) :: Nil => //single argument in single argument list
            Some(arg.name)
          case _ =>
            None
        }
  end Setter

end ClassAccessMacros
