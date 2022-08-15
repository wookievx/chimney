package io.scalaland.chimney.internal.utils.modules

import io.scalaland.chimney.internal.utils.ClassAccessMacros.{SimpleAccessor, Setter}
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

    def accessFrom[A: Type](value: Expr[A]): Expr[?] = this match
      case Simple(name, _) => 
        Select.unique(value.asTerm, name).asExpr
      case MethodField(_, methodName, _) =>
        val method = findGetterIn[A](methodName)
        Apply(Select(value.asTerm, method), List.empty).asExpr

    def patchWithValue[A: Type, AA: Type, B: Type](source: Expr[A], patchValue: Expr[B], patch: Expr[Patcher[AA, B]]): Term = 
      val accessor = accessFrom(source)
      '{ $patch.patch(${accessor.asExprOf[AA]}, $patchValue) }.asTerm
    
  end ReadField

  object ReadField:
    def fromMirror(mirror: ProductMirror): List[ReadField] = 
      mirror.fields.view.map((field, tpe) => Simple(field, tpe)).toList
    end fromMirror

    def fromTargetFields[A: Type](source: Expr[A], convertName: String => String, targetFields: List[TargetField]): List[ReadField] = 
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
    case BeanField(name: String, methodName: String, tpe: TypeRepr, target: Term)

    def name: String
    def tpe: TypeRepr

    def setValue[B: Type](value: Expr[B]): Term = this match
      case Simple(name, _) =>
        NamedArg(name, value.asTerm)
      case BeanField(_, methodName, _, target) =>
        val setter = findSetterIn[B](methodName)
        Apply(Select(target, setter), List(value.asTerm))
  
  end TargetField

  object TargetField:
    def fromMirror(mirror: ProductMirror): List[TargetField] = 
      mirror.fields.view.map((field, tpe) => Simple(field, tpe)).toList

    def fromSourceMirror[B: Type](sourceMirror: ProductMirror, convertName: String => String, target: Expr[B]): List[TargetField] =
      sourceMirror
        .fields
        .view
        .map { (field, _) =>
          val renamedField = convertName(field) 
          val method = findGetterIn[B](renamedField)
          val expectedType = Apply(Select(target.asTerm, method), List.empty).tpe
          BeanField(
            field,
            renamedField,
            expectedType,
            target.asTerm
          )
        }
        .toList
  end TargetField

  private def findGetterIn[A: Type](name: String): Symbol =
    val sym = TypeTree.of[A].symbol
    val methods: List[Symbol] = sym.declaredMethods
    methods.collectFirst { case s @ SimpleAccessor(name) => s }.getOrElse(report.errorAndAbort(s"Source class missing method for field: $name"))

  private def findSetterIn[A: Type](name: String): Symbol =
    val sym = TypeTree.of[A].symbol
    val methods: List[Symbol] = sym.declaredMethods
    methods.collectFirst { case s @ Setter(name) => s }.getOrElse(report.errorAndAbort(s"Target class missing value for field: $name"))

end FieldModule
