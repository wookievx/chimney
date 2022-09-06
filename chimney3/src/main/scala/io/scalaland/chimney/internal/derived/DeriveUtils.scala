package io.scalaland.chimney.internal.derived

object DeriveUtils:
  import scala.compiletime.ops.string.*

  type Concat[Path <: String, Field] <: String = Field match
    case String => Path + Field
    case _      => Path

end DeriveUtils
