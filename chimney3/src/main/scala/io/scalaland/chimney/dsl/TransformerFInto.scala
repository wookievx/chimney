package io.scalaland.chimney
package dsl

import io.scalaland.chimney.TransformerFSupport
import io.scalaland.chimney.internal._

import scala.compiletime._

final class TransformerFInto[F[_], From, To, Config <: Tuple, Flags <: Tuple](
  val source: From,
  val definition: TransformerFDefinition[F, From, To, Config, Flags]
) extends FlagsDsl[[FS <: Tuple] =>> TransformerFInto[F, From, To, Config, FS], Flags]:

  /** Use `value` provided here for field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#providing-missing-values]] for more details
    * @return [[io.scalaland.chimney.dsl.TransformerFInto]]
    */
  transparent inline def withFieldConst[T](inline selector: To => T, value: T) = 
    withDefinitionF(definition.withFieldConst(selector, value))

  /** Use wrapped `value` provided here for field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#providing-missing-values]] for more details
    * @param selector target field in `To`, defined like `_.name`
    * @param value    constant value to use for the target field
    * @return [[io.scalaland.chimney.dsl.TransformerFInto]]
    */
  transparent inline def withFieldConstF[T](inline selector: To => T, value: F[T]) = 
    withDefinitionF(definition.withFieldConstF(selector, value))

  /** Use `map` provided here to compute value of field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#providing-missing-values]] for more details
    * @param selector target field in `To`, defined like `_.name`
    * @param map      function used to compute value of the target field
    * @return [[io.scalaland.chimney.dsl.TransformerFInto]]
    */
  transparent inline def withFieldComputed[T](inline selector: To => T, map: From => T) = 
    withDefinitionF(definition.withFieldComputed(selector, map))
  
  /** Use `map` provided here to compute value of field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#providing-missing-values]] for more details
    * @param selector target field in `To`, defined like `_.name`
    * @param map      function used to compute value of the target field
    * @return [[io.scalaland.chimney.dsl.TransformerFInto]]
    */
  transparent inline def withFieldComputedF[T](inline selector: To => T, map: From => F[T]) = 
    withDefinitionF(definition.withFieldComputedF(selector, map))

  /** Use `selectorFrom` field in `From` to obtain the value of `selectorTo` field in `To`
    *
    * By default if `From` is missing field picked by `selectorTo` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#fields-renaming]] for more details
    * @param selectorFrom source field in `From`, defined like `_.originalName`
    * @param selectorTo   target field in `To`, defined like `_.newName`
    * @return [[io.scalaland.chimney.dsl.TransformerFInto]]
    */
  transparent inline def withFieldRenamed[T](inline selectorFrom: From => T, inline selectorTo: To => T) = 
    withDefinitionF(definition.withFieldRenamed(selectorFrom, selectorTo))

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
  inline def withCoproductInstance[FF <: From, TT <: To](f: FF => TT): TransformerFInto[F, From, To, EnableConfig[Config, TransformerCfg.CoproductInstance[FF, TT]], Flags] = 
    TransformerFInto(source, definition.withCoproductInstance(f))

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
    * @return [[io.scalaland.chimney.dsl.TransformerFDefinition]]
    */
  inline def withCoproductInstanceF[FF <: From, TT <: To](f: FF => F[TT]): TransformerFInto[F, From, To, EnableConfig[Config, TransformerCfg.CoproductInstanceF[FF, TT]], Flags] = 
    TransformerFInto(source, definition.withCoproductInstanceF(f))
  
  /**
   * For compatibilty with chimney for scala 2, transform defaults to looking-up existing instance of Transformer
  */
  inline def transform(using TransformerFSupport[F]): F[To] =
    summonFrom {
      case t: TransformerF[F, From, To] => t.transform(source)
      case _ =>  definition.buildTransformer.transform(source)
    }
  end transform

  transparent inline def withDefinitionF(inline newDefinition: Any) =
    inline newDefinition match
      case definition: TransformerFDefinition[F, From, To, config, flags] =>
        TransformerFInto(source, definition)
      case _ =>
        error("Changing definition failed, should not happen, a bug in library")

end TransformerFInto

extension[From](source: From)
  inline def intoF[F[_]: TransformerFSupport, To]: TransformerFInto[F, From, To, EnableConfig[EmptyTuple, TransformerCfg.WrapperType[F]], TransformerFlag.DefaultValues *: EmptyTuple] =
    TransformerInto(source, defaultDefinitionWithFlags[From, To, TransformerFlag.DefaultValues *: EmptyTuple]).lift[F]