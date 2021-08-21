package io.scalaland.chimney

import internal.derived.TransformerDerive
import internal.TransformerFlag
import dsl._
import scala.compiletime.error

trait Transformer[From, To]:
  def transform(from: From): To
end Transformer

object Transformer:

  inline def derived[From, To]: Transformer[From, To] = 
    TransformerDerive.derived[From, To, EmptyTuple, TransformerFlag.DefaultValues *: EmptyTuple](
      TransformerDefinition(Map.empty, Map.empty)
    )

  inline def defineF[F[_], From, To] = dsl.defaultDefinition[From, To].enableDefaultValues.lift[F]
  
  inline def define[From, To] = dsl.defaultDefinition[From, To].enableDefaultValues
end Transformer
