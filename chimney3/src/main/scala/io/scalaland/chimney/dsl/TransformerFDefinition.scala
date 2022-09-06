package io.scalaland.chimney.dsl

import io.scalaland.chimney.internal.TransformerFlag._
import io.scalaland.chimney.internal.dsl._
import io.scalaland.chimney.internal._
import io.scalaland.chimney.internal.derived.TransformerDerive
import io.scalaland.chimney._
import io.scalaland.chimney.internal.utils.MacroUtils
import scala.compiletime.error

/** Allows customization of [[io.scalaland.chimney.TransformerF]] derivation
  *
  * @tparam F    wrapper type constructor
  * @tparam From type of input value
  * @tparam To   type of output value
  * @tparam C    type-level encoded config
  */
final class TransformerFDefinition[F[_], From, To, Config <: Tuple, Flags <: Tuple](
    val overrides: Map[String, Any],
    val instances: Map[(String, String), Any]
) extends FlagsDsl[[FS <: Tuple] =>> TransformerFDefinition[F, From, To, Config, FS], Flags]:

/** Use `value` provided here for field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#providing-missing-values]] for more details
    * @param selector target field in `To`, defined like `_.name`
    * @param value    constant value to use for the target field
    * @return [[io.scalaland.chimney.dsl.TransformerFDefinition]]
    */
  transparent inline def withFieldConst[T, U](inline selector: To => T, value: U) = 
    TransformerFDefinitionBuilder.withFieldConst(this)(selector, value)

  /** Use wrapped `value` provided here for field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#providing-missing-values]] for more details
    * @param selector target field in `To`, defined like `_.name`
    * @param value    constant value to use for the target field
    * @return [[io.scalaland.chimney.dsl.TransformerFDefinition]]
    */
  transparent inline def withFieldConstF[T](inline selector: To => T, value: F[T]) = 
    TransformerFDefinitionBuilder.withFieldConstF(this)(selector, value)

  /** Use `map` provided here to compute value of field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#providing-missing-values]] for more details
    * @param selector target field in `To`, defined like `_.name`
    * @param map      function used to compute value of the target field
    * @return [[io.scalaland.chimney.dsl.TransformerFDefinition]]
    */
  transparent inline def withFieldComputed[T](inline selector: To => T, map: From => T) = 
    TransformerFDefinitionBuilder.withFieldComputed(this)(selector, map)

  /** Use `map` provided here to compute value of field picked using `selector`.
    *
    * By default if `From` is missing field picked by `selector` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#providing-missing-values]] for more details
    * @param selector target field in `To`, defined like `_.name`
    * @param map      function used to compute value of the target field
    * @return [[io.scalaland.chimney.dsl.TransformerFDefinition]]
    */
  transparent inline def withFieldComputedF[T](inline selector: To => T, map: From => F[T]) = 
    TransformerFDefinitionBuilder.withFieldComputedF(this)(selector, map)

  /** Use `selectorFrom` field in `From` to obtain the value of `selectorTo` field in `To`
    *
    * By default if `From` is missing field picked by `selectorTo` compilation fails.
    *
    * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#fields-renaming]] for more details
    * @param selectorFrom source field in `From`, defined like `_.originalName`
    * @param selectorTo   target field in `To`, defined like `_.newName`
    * @return [[io.scalaland.chimney.dsl.TransformerFDefinition]]
    */
  transparent inline def withFieldRenamed[T](inline selectorFrom: From => T,inline selectorTo: To => T) = 
    TransformerFDefinitionBuilder.withFieldRelabelled(this)(selectorFrom, selectorTo)

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
  inline def withCoproductInstance[FF <: From, TT <: To](f: FF => TT): TransformerFDefinition[F, From, To, EnableConfig[Config, TransformerCfg.CoproductInstance[FF, TT]], Flags] = 
    TransformerFDefinitionBuilder.withInstanceComputed(this)(f)

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
  inline def withCoproductInstanceF[FF <: From, TT <: To](f: FF => F[TT]): TransformerFDefinition[F, From, To, EnableConfig[Config, TransformerCfg.CoproductInstanceF[FF, TT]], Flags] = 
    TransformerFDefinitionBuilder.withInstanceComputedF(this)(f)

  /** Build Transformer using current configuration.
    *
    * It runs macro that tries to derive instance of `Transformer[From, To]`.
    * When transformation can't be derived, it results with compilation error.
    *
    * @return [[io.scalaland.chimney.TransformerF]] type class instance
    */
  inline def buildTransformer(using TransformerFSupport[F]): TransformerF[F, From, To] = 
    TransformerDerive.derivedF[F, From, To, Config, Flags](this)

end TransformerFDefinition