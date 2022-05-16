package lib.syntax

import cats.*
import cats.data.*
import cats.implicits.*

object StringSyntax:
  
  extension (string: String)
    def toBinaryOption: Option[Array[Boolean]] = 
      string.map { char  =>
        if char === '1' then Some(true) 
        else if char === '0' then Some(false) 
        else None
      }.toList.sequence.map(_.toArray)
    
    def toBinary: Array[Boolean] = toBinaryOption.get