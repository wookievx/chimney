package io.scalaland.chimney.utils

object EitherUtils {

  extension [T](opt: Option[T]) {
    def toEither(err: => String): Either[List[String], T] = {
      opt match {
        case Some(value) => Right(value)
        case None        => Left(List(err))
      }
    }
  }

}
