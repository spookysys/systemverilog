package com.github.svstuff.systemverilog
import spray.json._
import spray.json.DefaultJsonProtocol._

object Driver {


  type OptionMap = Map[Symbol, Any]
  def parseArgs(argsList : List[String]) : OptionMap = {
    val usageString = "Usage: svparse input -o output [--lex-only] [--analyze-complexity] [--dump-tokens] [--dump-parsetree]"
    def fail(error : String) = {
      println(error)
      println()
      println(usageString)
      sys.exit(1)
    }
    def isSwitch(s : String) = (s(0) == '-')
    def parseNext(map : OptionMap, list: List[String]) : OptionMap = {
      list match {
        case Nil => map
        case value :: tail if !isSwitch(value) =>
          parseNext(map ++ Map('input -> value), list.tail)
        case "-o" :: value :: tail =>
          parseNext(map ++ Map('output -> value), tail)
        case "--lex-only" :: tail =>
          parseNext(map ++ Map('lex_only -> true), tail)
        case "--analyze-complexity" :: tail =>
          parseNext(map ++ Map('analyze_complexity -> true), tail)
        case "--dump-tokens" :: tail =>
          parseNext(map ++ Map('dump_tokens -> true), tail)
        case "--dump-parsetree" :: tail =>
          parseNext(map ++ Map('dump_parsetree -> true), tail)
        case option :: tail =>
          fail("Unknown option "+option)
      }
    }
    val ret = parseNext(Map(), argsList)
    if (argsList.length == 0 || !(ret contains 'input) || !(ret contains 'output)) fail("Missing arguments")  
    return ret 
  }

  
  def main(args: Array[String]) {
    val options = parseArgs(args.toList)
    println(options)
  }

}