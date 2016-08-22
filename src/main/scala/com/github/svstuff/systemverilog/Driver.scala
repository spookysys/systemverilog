package com.github.svstuff.systemverilog

object Driver {


  type OptionMap = Map[Symbol, Any]
  def parseArgs(list : List[String]) : OptionMap = {
    def isSwitch(s : String) = (s(0) == '-')
    def parseNext(map : OptionMap, list: List[String]) : OptionMap = {
      list match {
        case Nil => map
        case "--lex-only" :: tail =>
          parseNext(map ++ Map('lexonly -> true), tail)
        case "-o" :: value :: tail =>
          parseNext(map ++ Map('output -> value), tail)
        case value :: tail if !isSwitch(value) =>
          parseNext(map ++ Map('input -> value), list.tail)
        case option :: tail =>
          println("Unknown option "+option) 
          sys.exit(1)            
      }
    }
    val defaults = Map(
      'lexonly -> false, 
      'input -> null, 
      'output -> null
    )
    return parseNext(defaults, list)
  }

  
  def main(args: Array[String]) {
    val usage = """
      Usage: svparse [-o output] [--lex-only] input
    """
    if (args.length == 0) println(usage)
    
    val options = parseArgs(args.toList)
    println(options)
  }

}