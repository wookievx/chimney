package io.scalaland.chimney.internal.derived

import io.scalaland.chimney.*
import io.scalaland.chimney.dsl.*

object TransformerDerive:

  inline def derived[From, To, Config <: Tuple, Flags <: Tuple](
    config: TransformerDefinition[From, To, Config, Flags]
  ): Transformer[From, To] = ${
    TransformerDeriveMacros.deriveTransformerMacro[From, To, Config, Flags]('{
      config
    })
  }

  inline def derivedF[
    F[_]: TransformerFSupport,
    From,
    To,
    Config <: Tuple,
    Flags <: Tuple
  ](
    config: TransformerFDefinition[F, From, To, Config, Flags]
  ): TransformerF[F, From, To] = ${
    TransformerDeriveMacros.deriveTransformerFMacro[F, From, To, Config, Flags](
      '{config},
      '{summon[TransformerFSupport[F]]}
    )
  }

end TransformerDerive
