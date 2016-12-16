package com.github.svstuff.systemverilog

import io.Source
import collection.JavaConversions._

import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree._
import org.antlr.v4.runtime.misc._
import org.antlr.v4.runtime.atn._

import java.util.concurrent._
import java.io.File
import java.io.PrintWriter

import generated._

object Driver2 {
  
  // knobs
  val tokenQueueLength = 1000

  // Options
  class Options (
      val sources : collection.immutable.Seq[String],
      val incDirs : collection.immutable.Seq[String],
      val defines : collection.immutable.Map[String, Option[String]],
      val output : Option[String],
      val analyzeComplexity : Option[String],
      val dumpTokens : Option[String],
      val dumpParseTree : Option[String],
      val lexOnly : Boolean,
      val logLevel : String
  )
  
  def drive(opts : Options) {
    
    // Set the default log level and formatting
    System.setProperty(org.slf4j.impl.SimpleLogger.DEFAULT_LOG_LEVEL_KEY, opts.logLevel)
    System.setProperty(org.slf4j.impl.SimpleLogger.SHOW_THREAD_NAME_KEY, "FALSE")
    System.setProperty(org.slf4j.impl.SimpleLogger.SHOW_SHORT_LOG_NAME_KEY, "TRUE")
    
    // Get logger
    val logger = org.slf4j.LoggerFactory.getLogger("Driver")
    
    /*
    The below is quite ugly, and should probably be redesigned. I'm concerned that using actors
    with very fine grained messages (tokens) will be too slow, hence the following pattern instead.

    An error in the lexer thread can be signalled to the parser by producing a special token. This
    will cause the parser thread to bail automatically. However, the lexer thread must interrupt the
    parser thread on unexpected (programmer error) exceptions.

    An error in the parser must explicitly stop the lexer thread, since otherwise the lexer will just
    fill up the blocking queue and sit and wait for the parser to consume a token.
    */
    
    
    // Create lexer and parser, connected by queue
    var tokenQueue = new TokenQueue(tokenQueueLength)
    var lexer = new Lexer2(tokenQueue)
    var parser = new Parser2(tokenQueue)
    
    // Create threads
    var lexerThread = new Thread(lexer)
    var parserThread = new Thread(parser)


    
    new Runnable {
      def runParser() {

        val tokenStream = new UnbufferedTokenStream(lexerWrapper)
        val parser = new generated.SVParser(tokenStream)

        // disable error recovery (causes NPE due to requiring a CharSource in the TokenStream)
        parser.setErrorHandler(new BailErrorStrategy())
        parser.setTrace( debugOptions.contains("trace") )
        parser.setBuildParseTree(true)

        if ( detectAmbiguations ){
          parser.getInterpreter().setPredictionMode(PredictionMode.LL_EXACT_AMBIG_DETECTION);
          parser.addErrorListener(new DiagnosticErrorListener())
        }

        val visitors = createVisitors(parser, features)
        visitors.foreach( _.start )

        try {

          // TODO run the semantic analysis and serializer (parsetree visitors) in a
          // separate thread by adding the parsetree for each root element to a queue.

          var prevtoktype = 0
          while( (prevtoktype != Token.EOF) && (prevtoktype != LexerTokens.ERROR) ){
            val parsetree = parser.root_element()
            visitors.foreach( _.visit(parsetree) )
            prevtoktype = parser.getCurrentToken().getType()
          }

          if ( prevtoktype != Token.EOF ){
            logger.error(s"Internal error on token type: ${prevtoktype}")
            sys.exit(1)
          }

          logger.info("SUCCESS")
        } catch {
          case e: ParseCancellationException => {
            val cause = e.getCause.asInstanceOf[RecognitionException]
            val tok = cause.getOffendingToken().asInstanceOf[SVToken]
            reportError(cause, tok)

            // TODO FIXME this is bad.
            // Instead return a Future or Promise or something and return this in the main function.
            sys.exit(1)
          }
          case e: CancellationException => {
            logger.info("Parser cancelled.")
          }
        }finally{
          visitors.foreach( _.finish )
        }

      }
      def reportError(e: RecognitionException, tok: SVToken){
        val sb = new StringBuilder
        val toktext = tok.getText
        sb ++= "Parsing failed. Found:%s".format(toktext)
        if ( tok.isEOF ) {
            sb ++= "\n"
        }else{
          val typetext = LexerTokens.tokenConstText(tok.getType)
          if ( toktext != typetext ) {
            sb ++= "(%s)\n".format(typetext)
          } else {
            sb ++= "\n"
          }
        }
        sb ++= "Expected one of: "
        for ( i <- e.getExpectedTokens.toList ){
          if ( i >= 0 ) {
            sb ++= "%s ".format(LexerTokens.tokenConstText(i))
          }else{
            sb ++= "<eof>"
          }
        }
        sb ++= "\n"
        if ( tok.ctx != null ){
          printContextChain(sb, tok.ctx, tok.line, tok.col)
        }
        logger.error(sb.toString)
      }
    })

    lexerThread = new Thread(new Runnable {
      override def run() {
        try {
          runLexer()
        } catch {
          case e: InterruptedException => parserThread.interrupt()
          case e: LexerError => {
            // NOTE: any lexer error will also produce an error token in the token stream, thus
            // stopping the parser naturally (via failure to match any rules).
            val sb = new StringBuilder
            sb ++= "Lexical error: %s\n".format(e.msg)
            printContextChain(sb, e.ctx, e.line, e.col)
            sb ++= e.ctx.what()
            logger.error(sb.toString)
            sys.exit(1)
          }
          case e: Throwable => {
            // make sure the parser is interrupted
            parserThread.interrupt()
            // rethrow the exception
            throw e
          }
        }
      }
      def runLexer() {
        // TODO output the actual DAG of file dependencies instead of just a flat list.
        val filesyaml = new PrintWriter("svparse_files.yml")
        val ctxListener = new FileContextListener {
          val seen = collection.mutable.Set[String]()
          override def enterSourceFile(ctx:Context){
            val added = seen.add(ctx.fileName)
            if ( added ) {
              filesyaml.write(s"- ${ctx.fileName}\n")
            }
          }
          override def enterInclude(currentCtx:Context, includeCtx:Context){
            enterSourceFile(includeCtx)
          }
          override def exitSourceFile(){}
          override def exitInclude(){}
        }
        val lexer = new OldieLexer(tokens, incdirs, printTokens, skipTokenEnqueue, Some(ctxListener))
        lexer.scan(sources, defines)
        filesyaml.close
      }
    })

    if ( debugOptions.contains("wait_stdin") ){
      // useful for profiling (connect to process before continuing)
      readLine()
    }

    lexerThread.start()

    if ( !lexerOnly ){
      parserThread.start()
    }
    
  }

  
  
  
  
  def toAbsolutePaths( prefix: String, paths: List[String] ) : List[String] = {
    paths.map( (path) => {
      if ( path(0) == '/' ) {
        path
      } else{
        "%s/%s".format(prefix, path)
      }
    })
  }
  
  
  def createVisitors(parser: Parser, features: List[String]) : List[SVVisitor] = {
    val visitors = new collection.mutable.ListBuffer[SVVisitor]
    if ( features.contains("block_style") ){
      visitors += new BlockStyleVisitor()
    }
    if ( features.contains("complexity") ){
      visitors += new ComplexityVisitor(parser, new PrintWriter("svparse.yml"))
    }
    if ( features.contains("serialize_to_xml") ){
      val fstream = new java.io.FileOutputStream("svparsetree.gz")
      val gzstream = new java.util.zip.GZIPOutputStream(fstream)
      visitors += new SerializerVisitor(parser, gzstream)
    }
    visitors.toList
  }
  
  def printContextChain(sb:StringBuilder, ctx:Context, line:Int, col:Int){
    sb ++= "In: %s(%d,%d)\n".format(ctx.where(), line, col)
    val what = ctx.what()
    if ( !what.isEmpty ){
      sb ++= "Context:\n"
      sb ++= what
      sb ++= "\n"
    }

    var parent = ctx.parent
    var child = ctx
    while ( parent != null ){
      sb ++= "Referenced from: %s(%d,%d)\n".format(parent.fileName, child.line, child.col)
      child = parent
      parent = parent.parent
    }
  }
}
