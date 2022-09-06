package io.scalaland.chimney.internal.derived

import scala.deriving._
import scala.compiletime._
import io.scalaland.chimney._
import io.scalaland.chimney.internal.utils.MacroUtils

object PatcherDerive:
  import DeriveUtils.Concat
  inline def derived[T, P, Config <: Tuple]: Patcher[T, P] =
    ${PatcherDeriveMacros.derive[T, P, Config]}

end PatcherDerive
