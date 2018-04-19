package de.thm.spl

object Main {
  def main(args: Array[String]): Unit = {
    def parseArguments(args: List[String], options: Compiler.Options): Option[Compiler.Options] = {
      val help =
        " Option     | Abbreviations     | Description             \n" +
          "------------+-------------------+-------------------------\n" +
          " --output   | -o                | Output file             \n" +
          " --input    | -i                | Input file              \n" +
          " --optimize | -opt              | Enable optimizations    \n" +
          " --help     | -h -?             | Show this help message   "
      args match {
        case "--output" :: path :: tail => parseArguments(tail, options.copy(output = Some(path)))
        case "-o" :: path :: tail => parseArguments(tail, options.copy(output = Some(path)))
        case "--input" :: path :: tail => parseArguments(tail, options.copy(input = Some(path)))
        case "-i" :: path :: tail => parseArguments(tail, options.copy(input = Some(path)))
        case "--optimize" :: tail => parseArguments(tail, options.copy(optimize = true))
        case "-opt" :: tail => parseArguments(tail, options.copy(optimize = true))
        case "--help" :: _ => println(help); None
        case "-h" :: _ => println(help); None
        case "-?" :: _ => println(help); None
        case Nil => Some(options)
        case other => println(s"Unknown compiler option '$other', see '-?' for more information!")
          None
      }
    }

    parseArguments(args.toList, Compiler.Options(None, None, optimize = false)) match {
      case Some(options) if options.input.isEmpty => println("Input file not specified")
      case Some(options) if options.output.isEmpty => println("Output file ist not specified")
      case Some(options) =>
        if (Compiler(options)) println("Compilation succeeded")
        else println("Compilation failed")
      case None =>
    }
  }
}
