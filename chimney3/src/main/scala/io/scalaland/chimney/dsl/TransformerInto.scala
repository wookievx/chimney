package io.scalaland.chimney
package dsl

import io.scalaland.chimney.internal.TransformerFlag._
import io.scalaland.chimney.internal._
import scala.compiletime._

final class TransformerInto[From, To, Config <: Tuple, Flags <: Tuple](
  val source: From,
  val definition: TransformerDefinition[From, To, Config, Flags]
) extends FlagsDsl[[FS <: Tuple] =>> TransformerInto[From, To, Config, FS], Flags]:

  /** Lifts current transformation with provided type constructor `F`.
    *
    * It keeps all the configuration, provided missing values, renames,
    * coproduct instances etc.
    *
    * @tparam F    wrapper type constructor
    * @return [[io.scalaland.chimney.dsl.TransformerFInto]]
    */
  inline def lift[F[_]]: TransformerFInto[F, From, To, EnableConfig[Config, TransformerCfg.WrapperType[F]], Flags] =
    TransformerFInto(source, definition.lift[F])

  /** Use `value` provided here for field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#providing-missing-values]] for more details
    * @return [[io.scalaland.chimney.dsl.TransformerInto]]
    */
  transparent inline def withFieldConst[T](inline selector: To => T, value: T) = 
    withDefinition(definition.withFieldConst(selector, value))

  /** Use wrapped `value` provided here for field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#providing-missing-values]] for more details
    * @param selector target field in `To`, defined like `_.name`
    * @param value    constant value to use for the target field
    * @return [[io.scalaland.chimney.dsl.TransformerInto]]
    */
  transparent inline def withFieldConstF[F[_], T](inline selector: To => T, value: F[T]) = 
    withDefinitionF[F](definition.withFieldConstF(selector, value))

  /** Use `map` provided here to compute value of field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#providing-missing-values]] for more details
    * @param selector target field in `To`, defined like `_.name`
    * @param map      function used to compute value of the target field
    * @return [[io.scalaland.chimney.dsl.TransformerInto]]
    */
  transparent inline def withFieldComputed[T](inline selector: To => T, map: From => T) = 
    withDefinition(definition.withFieldComputed(selector, map))
  
  /** Use `map` provided here to compute value of field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#providing-missing-values]] for more details
    * @param selector target field in `To`, defined like `_.name`
    * @param map      function used to compute value of the target field
    * @return [[io.scalaland.chimney.dsl.TransformerInto]]
    */
  transparent inline def withFieldComputedF[F[_], T](inline selector: To => T, map: From => F[T]) = 
    withDefinitionF[F](definition.withFieldComputedF(selector, map))

  /** Use `selectorFrom` field in `From` to obtain the value of `selectorTo` field in `To`
    *
    * By default if `From` is missing field picked by `selectorTo` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#fields-renaming]] for more details
    * @param selectorFrom source field in `From`, defined like `_.originalName`
    * @param selectorTo   target field in `To`, defined like `_.newName`
    * @return [[io.scalaland.chimney.dsl.TransformerInto]]
    */
  transparent inline def withFieldRenamed[T](inline selectorFrom: From => T, inline selectorTo: To => T) = 
    withDefinition(definition.withFieldRenamed(selectorFrom, selectorTo))

  /** Use `f` to calculate the (missing) coproduct instance when mapping one coproduct into another.
    *
    * By default if mapping one coproduct in `From` into another coproduct in `To` derivation
    * expects that coproducts to have matching names of its components, and for every component
    * in `To` field's type there is matching component in `From` type. If some component is missing
    * it fails compilation unless provided replacement with this operation.
    * This method might require passing specific types explictly like:
    * ```scala
    * definition.withCoproductInstance[CaseA, CaseB](_ => ???)
    * ```
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#transforming-coproducts]] for more details
    * @param f function to calculate values of components that cannot be mapped automatically
    * @return [[io.scalaland.chimney.dsl.TransformerInto]]
    */
  inline def withCoproductInstance[FF <: From, TT <: To](f: FF => TT): TransformerInto[From, To, EnableConfig[Config, TransformerCfg.CoproductInstance[FF, TT]], Flags] = 
    TransformerInto(source, definition.withCoproductInstance(f))

  /** Use `f` to calculate the (missing) coproduct instance when mapping one coproduct into another.
    *
    * By default if mapping one coproduct in `From` into another coproduct in `To` derivation
    * expects that coproducts to have matching names of its components, and for every component
    * in `To` field's type there is matching component in `From` type. If some component is missing
    * it fails compilation unless provided replacement with this operation.
    * This method might require passing specific types explictly like:
    * ```scala
    * definition.withCoproductInstance[CaseA, CaseB](_ => ???)
    * ```
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#transforming-coproducts]] for more details
    * @param f function to calculate values of components that cannot be mapped automatically
    * @return [[io.scalaland.chimney.dsl.TransformerFInto]]
    */
  transparent inline def withCoproductInstanceF[F[_], FF <: From, TT <: To](f: FF => F[TT]) = 
    lift[F].withCoproductInstanceF[FF, TT](f)

  /**
   * For compatibilty with chimney for scala 2, transform defaults to looking-up existing instance of Transformer
  */
  inline def transform: To =
    summonFrom {
      case t: Transformer[From, To] => 
        t.transform(source)
      case _ => 
        definition.buildTransformer.transform(source)
    }
  end transform

  transparent inline def withDefinition(inline newDefinition: Any) =
    inline newDefinition match
      case definition: TransformerDefinition[From, To, config, flags] =>
        TransformerInto(source, definition)
      case _ =>
        error("Changing definition failed, should not happen, a bug in library")

  transparent inline def withDefinitionF[F[_]](inline newDefinition: Any) =
    inline newDefinition match
      case definition: TransformerFDefinition[F, From, To, config, flags] =>
        TransformerFInto(source, definition)
      case _ =>
        error("Changing definition failed, should not happen, a bug in library")

end TransformerInto

extension[From](source: From)
  inline def into[To]: TransformerInto[From, To, EmptyTuple, EmptyTuple] =
    TransformerInto(source, defaultDefinition[From, To])
