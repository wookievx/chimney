package io.scalaland.chimney.dsl

import io.scalaland.chimney.internal.TransformerFlag._
import io.scalaland.chimney.internal._

class TransformerConfiguration[Flags <: Tuple]
    extends FlagsDsl[[F1 <: Tuple] =>> TransformerConfiguration[F1], Flags]

object TransformerConfiguration:
  given default: TransformerConfiguration[EmptyTuple] = new TransformerConfiguration[EmptyTuple]
end TransformerConfiguration