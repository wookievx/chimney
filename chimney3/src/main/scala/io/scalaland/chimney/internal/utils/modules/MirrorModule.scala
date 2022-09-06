package io.scalaland.chimney.internal.utils.modules

import scala.annotation.tailrec
import scala.quoted.*
import scala.deriving.*
import scala.util.chaining.*

trait MirrorModule:
  self: Module =>
  import quotes.reflect.*

  case class ProductMirror(fields: Map[String, TypeRepr]):
    def filter(regularField: String => Boolean): ProductMirror = ProductMirror(
      fields.filter((k, _) => regularField(k))
    )

  object ProductMirror:

    def fromMirror(mirror: Expr[Mirror.Product]): ProductMirror = {
      val mirrorTpe = mirror.asTerm.tpe.widen
      val labels = findMemberType(mirrorTpe, "MirroredElemLabels")
      val elemTypes =
        findMemberType(mirrorTpe, "MirroredElemTypes").map(tupleTypeElements)
      val labelValues = labels.map(tupleTypeElements(_).map { repr =>
        repr.asType match
          case '[t] =>
            Type.valueOfConstant[t] match
              case Some(l: String) => l
              case l => report.errorAndAbort(s"Illegal label const value: $l")
      })

      labelValues
        .zip(elemTypes)
        .map((labelValues, elemTypes) =>
          ProductMirror(labelValues.zip(elemTypes).toMap)
        )
        .getOrElse(ProductMirror(Map.empty))
    }

  case class CoproductMirror[T](
    cases: List[(Int, (String, TypeRepr))],
    caseOrdinality: Expr[T => Int]
  )

  object CoproductMirror {
    def fromMirror[T: Type](
      mirror: Expr[Mirror.SumOf[T]]
    ): CoproductMirror[T] = {
      val mirrorTpe = mirror.asTerm.tpe.widen
      val labels = findMemberType(mirrorTpe, "MirroredElemLabels").getOrElse(
        report.errorAndAbort(
          "Mirror missing MirroredElemLabels, should not happen"
        )
      )
      val elemTypes = findMemberType(mirrorTpe, "MirroredElemTypes")
        .getOrElse(
          report.errorAndAbort(
            "Mirror missing MirroredElemTypes, should not happen"
          )
        )
        .pipe(tupleTypeElements)

      val labelValues = tupleTypeElements(labels).map { repr =>
        repr.asType match
          case '[t] =>
            Type.valueOfConstant[t] match
              case Some(l: String) => l
              case l => report.errorAndAbort(s"Illegal label const value: $l")
      }

      CoproductMirror(
        cases = List.range(0, labelValues.size).zip(labelValues.zip(elemTypes)),
        caseOrdinality = '{ (t: T) => $mirror.ordinal(t) }
      )

    }
  }

  private def tupleTypeElements(tp: TypeRepr): List[TypeRepr] = {
    @tailrec def loop(tp: TypeRepr, acc: List[TypeRepr]): List[TypeRepr] =
      tp match {
        case AppliedType(pairTpe, List(hd: TypeRepr, tl: TypeRepr)) =>
          loop(tl, hd :: acc)
        case _ => acc
      }

    loop(tp, Nil).reverse
  }

  private def findMemberType(tp: TypeRepr, name: String): Option[TypeRepr] =
    tp match {
      case Refinement(_, `name`, tp) => Some(low(tp))
      case Refinement(parent, _, _)  => findMemberType(parent, name)
      case AndType(left, right) =>
        findMemberType(left, name).orElse(findMemberType(right, name))
      case _ => None
    }

  private def low(tp: TypeRepr): TypeRepr = tp match {
    case tp: TypeBounds => tp.low
    case tp             => tp
  }

  private[internal] def constructor[B: Type]: Term =
    val tpe = TypeRepr.of[B]
    val (repr, constructor, tpeArgs) = tpe match
      case AppliedType(repr, reprArguments) =>
        (repr, repr.typeSymbol.primaryConstructor, reprArguments)
      case notApplied => (tpe, tpe.typeSymbol.primaryConstructor, Nil)

    New(Inferred(repr))
      .select(constructor)
      .appliedToTypes(tpeArgs)
  end constructor

end MirrorModule
