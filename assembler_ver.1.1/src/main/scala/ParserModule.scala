trait ParserModule:
    def moldAssembly(assembly: Traversable[String]) = 
        assembly.map(_.split("//").headOption.map(_.filter(_!=' ')))

