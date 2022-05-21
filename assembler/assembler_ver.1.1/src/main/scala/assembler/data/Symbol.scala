package assembler.data

import cats._
import cats.data._
import cats.implicits._


enum Symbol(val address: Int):
    case SP     extends Symbol(0)
    case LCL    extends Symbol(1)
    case ARG    extends Symbol(2)
    case THIS   extends Symbol(3)
    case THAT   extends Symbol(4)
    case SCREEN extends Symbol(16384)
    case KBD    extends Symbol(24576)
    