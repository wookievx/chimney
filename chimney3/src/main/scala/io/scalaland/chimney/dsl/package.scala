package io.scalaland.chimney
package dsl

import scala.compiletime.summonFrom

extension [From](from: From)
  inline def transformInto[To]: To = 
    summonFrom {
      case t: Transformer[From, To] =>
        t.transform(from)
      case _ => 
        Transformer.derived[From, To].transform(from)
    }
  inline def transformIntoF[F[_], To]: F[To] = 
    summonFrom {
      case t: TransformerF[F, From, To] =>
        t.transform(from)
      case _ =>
        summonFrom {
          case s: TransformerFSupport[F] =>
            TransformerF.derived[F, From, To](using s).transform(from)
        }
    }
