package com.github.svstuff.systemverilog
import spray.json._
import spray.json.DefaultJsonProtocol._



object CommandLineRunner {

  // regex's for option parser
  private val switchPattern = "^([+-].*)$".r
  private val noSwitchPattern = "^([^+-].*)$".r
  private val incDirPattern = "^(\\+incdir\\+|-I)(.+)$".r
  private val macroPattern = "^(\\+define\\+|-D)([^=]+)$".r
  private val macroValuePattern = "^(\\+define\\+|-D)([^=]+)=([^=]+)$".r
  
  // failure handler
  private val usageString = "Usage: svparse source.sv [-o output.json] [--analyze-complexity output] [--dump-tokens output [--lex-only]] [--dump-parsetree output.xml.gz] ..."
  private def fail(error : String) = {
    println(error)
    println()
    println(usageString)
    sys.exit(1)
  }

  // program entry point
  def main(args : Array[String]) {
    
    // Install default exception handler
    Thread.setDefaultUncaughtExceptionHandler(new Thread.UncaughtExceptionHandler {
      override def uncaughtException(t: Thread, e: Throwable){
        e.printStackTrace()
        sys.exit(1)
      }
    })
    
    // data goes here
    var sources = collection.mutable.LinkedHashSet[String]() 
    var incDirs = collection.mutable.LinkedHashSet[String]()
    var defines = collection.mutable.LinkedHashMap[String, Option[String]]()
    var output : Option[String] = null
    var analyzeComplexity : Option[String] = null
    var dumpTokens : Option[String] = null
    var dumpParseTree : Option[String] = null
    var lexOnly = false
    var logLevel = "DEBUG"

    // parse options
    def parseArgs(args : List[String]) : Unit = {
      val tail = args match {
       case noSwitchPattern(filename) :: tail =>
          sources += filename
          tail
        case incDirPattern(flag, directory) :: tail => 
          incDirs += directory
          tail
        case macroPattern(flag, name) :: tail =>
          defines(name) = null
          tail
        case macroValuePattern(flag, name, value) :: tail =>
          defines(name) = Some(value)
          tail
        case "-o" :: noSwitchPattern(filename) :: tail =>
          output = Some(filename)
          tail
        case "--analyze-complexity" :: noSwitchPattern(filename) :: tail =>
          analyzeComplexity = Some(filename)
          tail
        case "--dump-tokens" :: noSwitchPattern(filename) :: tail =>
          dumpTokens = Some(filename)
          tail
        case "--dump-parsetree" :: noSwitchPattern(filename) :: tail =>
          dumpParseTree = Some(filename)
          tail
        case "--lex-only" :: tail =>
          lexOnly = true
          tail
        case "--log-level" :: value :: tail =>
          logLevel = value
          tail
        case option :: tail =>
          fail("Unknown option "+option)
          tail
        case Nil =>
          if (sources.size == 0) fail("Please specify one or more source files")
          Nil
        }
      parseArgs(tail)
    }
    parseArgs(args.toList)

    
    // Embed result in object 
    val opts = new Driver2.Options (
        collection.immutable.Seq(sources.toSeq:_*),
        collection.immutable.Seq(incDirs.toSeq:_*),
        defines.toMap,
        output,
        analyzeComplexity,
        dumpTokens,
        dumpParseTree,
        lexOnly,
        logLevel
    )

    
    // Call driver
    Driver2.drive(opts)
  }

  

}