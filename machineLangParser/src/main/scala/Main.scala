object Main {
  def main(args: Array[String]): Unit = args match {
    // only 16 bit command.
    case args if args.length == 1 => args.head match {
      case input if input.length == 16 &&
        input.forall(c => c.equals('0') || c.equals('1')) =>
        println(
         s"""
            |[RESULT]
            |15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
            |${
             if (input.head.equals('1')) {
               " 1  1  1  a  c  c  c  c  c  c  d  d  d  j  j  j"
             } else {
               " 0  v  v  v  v  v  v  v  v  v  v  v  v  v  v  v"
             }}
            |${input.foldLeft(" ")((acc,x) => acc + x + "  ")}
            |""".stripMargin)
      case _ => System.err.println("Invalid argument value.");
    }
    case _ => System.err.println("Invalid arguments length.");
  }
}
