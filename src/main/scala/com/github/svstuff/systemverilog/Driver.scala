package com.github.svstuff.systemverilog
import scala.collection._
import spray.json._
import spray.json.DefaultJsonProtocol._



object Driver {

  class Options (
      val sources : Set[String],
      val incDirs : Set[String],
      val defines : Map[String, Option[String]],
      val output : Option[String],
      val analyzeComplexity : Option[String],
      val dumpTokens : Option[String],
      val dumpParseTree : Option[String],
      val lexOnly : Boolean
  )
  
  def parseArgs(argsList : List[String]) : Options = {

    // failure
    val usageString = "Usage: svparse source.sv [-o output.json] [--analyze-complexity output] [--dump-tokens output [--lex-only]] [--dump-parsetree output.xml.gz] ..."
    def fail(error : String) = {
      println(error)
      println()
      println(usageString)
      sys.exit(1)
    }

    // data goes here
    var sources : mutable.Set[String] = mutable.LinkedHashSet()
    var incDirs : mutable.Set[String] = mutable.LinkedHashSet()
    var defines : mutable.Map[String, Option[String]] = mutable.LinkedHashMap[String, Option[String]]()
    var output : Option[String] = null
    var analyzeComplexity : Option[String] = null
    var dumpTokens : Option[String] = null
    var dumpParseTree : Option[String] = null
    var lexOnly = false

    // compiled regex's for option parser
    val incDirPattern = "^(\\+incdir\\+|-I)(.+)$".r
    val macroPattern = "^(\\+define\\+|-D)([^=]+)$".r
    val macroValuePattern = "^(\\+define\\+|-D)([^=]+)=([^=]+)$".r
    
    // define recursive option parser
    def parseNext(list: List[String]) : Unit = {
      def isSwitch(s : String) = (s(0) == '-' || s(0) == '+')
      list match {
       case filename :: tail if !isSwitch(filename) =>
          sources += filename
          parseNext(tail)
        case incDirPattern(flag, directory) :: tail => 
          incDirs += directory
          parseNext(tail)
        case macroPattern(flag, name) :: tail =>
          defines(name) = null
          parseNext(tail)
        case macroValuePattern(flag, name, value) :: tail =>
          defines(name) = Some(value)
          parseNext(tail)
        case "-o" :: filename :: tail =>
          output = Some(filename)
          parseNext(tail)
        case "--analyze-complexity" :: filename :: tail =>
          analyzeComplexity = Some(filename)
          parseNext(tail)
        case "--dump-tokens" :: filename :: tail =>
          dumpTokens = Some(filename)
          parseNext(tail)
        case "--dump-parsetree" :: filename :: tail =>
          dumpParseTree = Some(filename)
          parseNext(tail)
        case "--lex-only" :: tail =>
          lexOnly = true
          parseNext(tail)
        case option :: tail =>
          fail("Unknown option "+option)
        case Nil =>
          if (sources.size == 0) fail("Please specify one or more source files")
        }
    }

    // run the parser
    parseNext(argsList)
    
    // return the data
    return new Options (
        sources,
        incDirs,
        defines,
        output,
        analyzeComplexity,
        dumpTokens,
        dumpParseTree,
        lexOnly
    )
  }

  
  def main(args: Array[String]) {
    val options = parseArgs(args.toList)
    println(options.sources)    
    println(options.incDirs)    
    println(options.defines)    
  }

}