package io.scalaland.chimney.internal.utils

import scala.quoted.*
import deriving.*, compiletime.*

import java.time.Instant

object MacroUtils:

  inline def getDefaultParams[T]: Map[String, AnyRef] = ${
    getDefaultParmasImpl[T]
  }
  //copied from : https://github.com/dotty-staging/upickle/blob/0213eea95b282b1e961b1d5ad68031365c9a8bb2/implicits/src-3/upickle/implicits/macros.scala
  def getDefaultParmasImpl[T: Type](using Quotes): Expr[Map[String, AnyRef]] =
    import quotes.reflect.*
    val sym = TypeTree.of[T].symbol

    if (sym.isClassDef) {
      val comp = if (sym.isClassDef) sym.companionClass else sym
      val names =
        for p <- sym.caseFields if p.flags.is(Flags.HasDefault)
        yield p.name
      val namesExpr: Expr[List[String]] =
        Expr.ofList(names.map(Expr(_)))

      val body =
        try {
          comp.tree.asInstanceOf[ClassDef].body
        } catch {
          case _ =>
            List.empty
        }
      val idents: List[Ref] =
        for
          case deff @ DefDef(name, _, _, _) <- body
          if name.startsWith("$lessinit$greater$default")
        yield Ref(deff.symbol)
      val identsExpr: Expr[List[Any]] =
        Expr.ofList(idents.map(_.asExpr))

      '{ $namesExpr.zip($identsExpr.map(_.asInstanceOf[AnyRef])).toMap }
    } else {
      '{ Map.empty }
    }
  end getDefaultParmasImpl

  inline def defaultValueExistsIn[T](inline name: Any): Boolean = ${
    nameExistsInImpl[T]('name)
  }

  def nameExistsInImpl[T: Type](name: Expr[Any])(using Quotes): Expr[Boolean] =
    import quotes.reflect.*
    val sym = TypeTree.of[T].symbol

    if (sym.isClassDef) {
      val comp = if (sym.isClassDef) sym.companionClass else sym
      val names =
        for p <- sym.caseFields if p.flags.is(Flags.HasDefault)
        yield p.name

      name match
        case '{ $n: String } =>
          n.value match
            case Some(name) =>
              Expr(names.contains(name))
            case _ =>
              report.throwError(
                "Failed to check if name exist, probably a bug in library"
              )
        case _ =>
          report.throwError(
            "Failed to check if name exist, probably a bug in library"
          )
    } else {
      Expr(false)
    }
  end nameExistsInImpl

  transparent inline def extracNameFromSelector[To, T](inline code: To => T) =
    ${ extractNameFromSelectorImpl('code) }

  def extractNameFromSelectorImpl[To: Type, T: Type](code: Expr[To => T])(using
    Quotes
  ): Expr[String] =
    import quotes.reflect.*
    code.asTerm match
      case InlinedLambda(
            List(ValDef(identVal, _, _)),
            t @ Select(Ident(identExtract), name)
          ) if identVal == identExtract =>
        Expr(name)
      case t =>
        report.throwError(
          s"Illegal selector: ${normalizeLambdaMessage(code.show)}"
        )

  object InlinedLambda:
    def unapply(using Quotes)(
      arg: quotes.reflect.Term
    ): Option[(List[quotes.reflect.ValDef], quotes.reflect.Term)] =
      import quotes.reflect.*
      arg match
        case Inlined(_, _, Lambda(vals, term)) => Some((vals, term))
        case Inlined(_, _, nested)             => InlinedLambda.unapply(nested)
        case t                                 => None
  end InlinedLambda

  private def normalizeLambdaMessage(lambdaShow: String): String =
    lambdaShow.replaceAll("""_\$\d+""", "x")
  end normalizeLambdaMessage

  inline def debug[T](inline any: T): T = ${ printImplMacro('any) }

  def printImplMacro[T: Type](any: Expr[T])(using qctx: Quotes): Expr[T] = {
    import qctx.reflect.*
    report.info(Printer.TreeShortCode.show(any.asTerm).toString)
    any
  }

  inline def debugCodeTree[T](inline any: T): T = ${ debugCodeTreeMacro('any) }

  def debugCodeTreeMacro[T: Type](any: Expr[T])(using Quotes): Expr[T] =
    import quotes.reflect.*
    any.asTerm match
      case Inlined(_, _, code) =>
        report.info(code.toString)
      case code =>
        report.info(code.toString)
    any
  end debugCodeTreeMacro

  transparent inline def showTypeVal[T] = ${ showTypeImpl[T] }

  def showTypeExpr[T: Type](using quotes: Quotes): Expr[String] =
    showTypeImpl[T]

  def showTypeImpl[T: Type](using quotes: Quotes): Expr[String] =
    Expr(Type.show[T])

  inline def showType[T]: Unit = ${ printType[T] }

  def showTypeMacro[T: Type](using Quotes): Unit =
    println(s"Got type: ${Type.show[T]}")

  private def printType[T: Type](using quotes: Quotes): Expr[Unit] =
    println(s"Got type: ${Type.show[T]}")
    '{}
  end printType

  transparent inline def summonProductOf[T] = ${ attemptSummonMirror[T] }

  private def attemptSummonMirror[T: Type](using q: Quotes): Expr[Any] = {
    import q.reflect.report
    Expr.summon[Mirror.ProductOf[T]] match
      case Some(product) => product
      case None =>
        report.throwError(s"Failed to summon product of t: ${Type.show[T]}")
  }

  transparent inline def attemptSummonInstance[T] = ${
    attemptSummonInstanceImpl[T]
  }

  private def attemptSummonInstanceImpl[T: Type](using
    q: Quotes
  ): Expr[Option[T]] = {
    import q.reflect.report
    Expr.summon[T] match
      case Some(instance) => '{ Some($instance) }
      case None           => '{ None }
  }

  inline def reportErrorAtPathWithType[P <: String, Error <: String, T](
    inline constantPart: String
  ) = ${ reportErrorAtPathWithTypeImpl[P, Error, T]('constantPart) }

  inline def reportErrorAtPath[P <: String](
    inline path: P,
    inline constantPart: String
  ) = ${ reportErrorAtPathMacro('path, 'constantPart) }

  private def reportErrorAtPathWithTypeImpl[
    P <: String: Type,
    Error <: String: Type,
    T: Type
  ](constantPart: Expr[String])(using q: Quotes): Expr[Nothing] =
    import q.reflect.report
    (
      Type.valueOfConstant[P],
      constantPart.value,
      Type.valueOfConstant[Error]
    ) match
      case (Some(path), Some(constantPart), Some(error)) =>
        report.throwError(
          s"$constantPart at $path, type in question: ${Type.show[T]}, error: ${error}"
        )
      case (p, cp, e) =>
        report.error(s"WTF: got $p, $cp, $e")
        report.throwError("Unable to produce nice error, bug in library")

  end reportErrorAtPathWithTypeImpl

  def reportErrorAtPathMacro[P <: String](
    path: Expr[P],
    constantPart: Expr[String]
  )(using q: Quotes): Expr[Nothing] = {
    import q.reflect.report
    (path.value, constantPart.value) match
      case (Some(path), Some(v)) =>
        report.throwError(s"$v at $path")
      case (p, cp) =>
        report.error(s"WTF: got $p, $cp")
        report.throwError("Unable to produce nice error, bug in library")
  }

  inline def reportPointOfDerivation[P <: String](inline path: P) = ${
    reportPointOfDerivationImpl('path)
  }

  private def reportPointOfDerivationImpl[P <: String](
    path: Expr[P]
  )(using Quotes): Expr[Unit] = {
    path.value match
      case Some(path) =>
        println(s"Automatic derivation at $path")
        '{}
      case None =>
        '{}
  }

  inline def printAtCompileTime[P <: String] = ${ printAtCompileTimeImpl[P] }

  private def printAtCompileTimeImpl[P <: String: Type](using
    Quotes
  ): Expr[Unit] =
    Type.valueOfConstant[P] match
      case Some(p) =>
        println(p)
        '{}
      case None =>
        '{}

  inline def doPrintFCompileTime[P <: String, Args <: Tuple]: Unit = ${
    doPrintFCompileTimeImpl[P, Args]
  }

  private def doPrintFCompileTimeImpl[P <: String: Type, Args <: Tuple: Type](
    using Quotes
  ): Expr[Unit] =
    printfCompileTimeMacro[P, Args].value match
      case Some(s) =>
        println(s)
        '{}
      case None =>
        '{}
  end doPrintFCompileTimeImpl

  inline def printfCompileTime[P <: String, Args <: Tuple]: String = ${
    printfCompileTimeMacro[P, Args]
  }

  def printfCompileTimeMacro[P <: String: Type, Args <: Tuple: Type](using
    Quotes
  ): Expr[String] =
    Type.valueOfConstant[P] match
      case Some(v) =>
        Expr(v.format(showAll[Args]: _*))
      case None =>
        quotes.reflect.report.throwError(
          "Failed to format string at compile time, internal library error"
        )
  end printfCompileTimeMacro

  inline def reportCompilationTime: Unit = ${ reportCompilationTimeMacro }

  def reportCompilationTimeMacro(using Quotes): Expr[Unit] =
    println(Instant.now)
    '{}

  private def showAll[Args <: Tuple: Type](using Quotes): List[String] =
    Type.of[Args] match
      case '[arg *: args] =>
        Type.show[arg] :: showAll[args]
      case _ =>
        List.empty
  end showAll

  inline def showTermOfCode(inline arg: Any): String = ${
    showTermOfCodeImpl('arg)
  }

  private def showTermOfCodeImpl(arg: Expr[Any])(using Quotes): Expr[String] = {
    import quotes.reflect.*
    Expr(arg.asTerm.toString)
  }

end MacroUtils
