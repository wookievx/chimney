package io.scalaland.chimney.internal.utils.modules

import io.scalaland.chimney.internal.utils.ClassAccessMacros.{
  SimpleAccessor,
  Setter
}
import io.scalaland.chimney.Patcher
import scala.quoted.*

trait FieldModule:
  self: Module & MirrorModule =>
  import quotes.reflect.*

  enum ReadField:
    case Simple(name: String, tpe: TypeRepr)
    case MethodField(name: String, methodName: String, tpe: TypeRepr)

    def name: String
    def tpe: TypeRepr

    //widening returned value, not supporting path dependent types
    def accessFrom[A: Type](value: Expr[A]): Expr[?] = this match
      case Simple(name, tpe) =>
        tpe.asType match
          case '[t] =>
            Select.unique(value.asTerm, name).asExprOf[t]
      case MethodField(_, methodName, tpe) =>
        val method = findGetterIn[A](methodName)
        tpe.asType match
          case '[t] =>
            Apply(Select(value.asTerm, method), List.empty).asExprOf[t]

    def patchWithValue[A: Type, AA: Type, B: Type](
      source: Expr[A],
      patchValue: Expr[B],
      patch: Expr[Patcher[AA, B]]
    ): Term =
      val accessor = accessFrom(source)
      '{ $patch.patch(${ accessor.asExprOf[AA] }, $patchValue) }.asTerm

  end ReadField

  object ReadField:
    def fromMirror(mirror: ProductMirror): List[ReadField] =
      mirror.fields.view.map((field, tpe) => Simple(field, tpe)).toList
    end fromMirror

    def fromTargetFields[A: Type](
      source: Expr[A],
      convertName: String => String,
      targetFields: List[TargetField]
    ): List[ReadField] =
      targetFields.view.map { field =>
        val renamedField = convertName(field.name)
        val method = findGetterIn[A](renamedField)
        MethodField(
          field.name,
          renamedField,
          Apply(Select(source.asTerm, method), List.empty).tpe
        )
      }.toList
    end fromTargetFields

  end ReadField

  enum TargetField:
    case Simple(name: String, tpe: TypeRepr)
    case BeanField(
      name: String,
      methodName: String,
      tpe: TypeRepr,
      target: Term
    )

    def name: String
    def tpe: TypeRepr

    def setValue[B: Type](value: Expr[B]): Term = this match
      case Simple(name, _) =>
        NamedArg(name, value.asTerm)
      case BeanField(_, methodName, tpe, target) =>
        tpe.asType match
          case '[b] =>
            val setter = findSetterIn[b](methodName)
            Apply(Select(target, setter), List(value.asTerm))

    //for deriving `TransformerF`, for now to simplify macro code
    def setValueUnsafe[B: Type](value: Expr[B]): Term = this match
      case Simple(name, tpe) =>
        tpe.asType match
          case '[b] => NamedArg(name, '{ $value.asInstanceOf[b] }.asTerm)
      case BeanField(_, methodName, tpe, target) =>
        tpe.asType match
          case '[b] =>
            val setter = findSetterIn[b](methodName)
            Apply(Select(target, setter), List(value.asTerm))

    def setNone: Option[Term] = this match
      case Simple(name, tpe) =>
        tpe.asType match
          case '[Option[b]] =>
            Some(NamedArg(name, '{ Option.empty[b] }.asTerm))
          case '[t] =>
            println(s"Got tpe of the field: ${Type.show[t]}")
            None
      case BeanField(_, methodName, tpe, target) =>
        tpe.asType match
          case '[Option[b]] =>
            val setter = findSetterIn[b](methodName)
            Some(
              Apply(Select(target, setter), List('{ Option.empty[b] }.asTerm))
            )
          case _ =>
            None

  end TargetField

  object TargetField:
    def fromMirror(mirror: ProductMirror): List[TargetField] =
      mirror.fields.view.map((field, tpe) => Simple(field, tpe)).toList

    def fromSourceMirror[B: Type](
      sourceMirror: ProductMirror,
      convertName: String => String,
      target: Expr[B]
    ): List[TargetField] =
      sourceMirror.fields.view.map { (field, _) =>
        val renamedField = convertName(field)
        val method = findGetterIn[B](renamedField)
        val expectedType = Apply(Select(target.asTerm, method), List.empty).tpe
        BeanField(
          field,
          renamedField,
          expectedType,
          target.asTerm
        )
      }.toList
  end TargetField

  private def findGetterIn[A: Type](name: String): Symbol =
    val sym = TypeTree.of[A].symbol
    val methods: List[Symbol] = sym.declaredMethods
    methods
      .collectFirst { case s @ SimpleAccessor(`name`) => s }
      .getOrElse(
        report.errorAndAbort(s"Source class missing method for field: $name")
      )

  private def findSetterIn[A: Type](name: String): Symbol =
    val sym = TypeTree.of[A].symbol
    val methods: List[Symbol] = sym.declaredMethods
    methods
      .collectFirst { case s @ Setter(name) => s }
      .getOrElse(
        report.errorAndAbort(s"Target class missing value for field: $name")
      )

end FieldModule
