package com.github.svstuff.systemverilog


class TokenQueue(tokenQueueCapacity : Int) {
  
  private class TokenSource(tokens : java.util.concurrent.BlockingQueue[SVToken]) extends org.antlr.v4.runtime.TokenSource {
    override def nextToken() : Token = tokens.take()
    override def setTokenFactory(factory : TokenFactory[_]) = {}
    override def getTokenFactory() : TokenFactory[_] = { null }
    override def getSourceName() : String = { "TokenQueue" }
    override def getInputStream() : CharStream = { null }
    override def getCharPositionInLine() : Int = { 0 }
    override def getLine() : Int = { 0 }
  }

  private val blockingQueue = new java.util.concurrent.ArrayBlockingQueue[SVToken](tokenQueueCapacity)
  private val tokenSource = new TokenSource(blockingQueue)
  private val tokenStream = new UnbufferedTokenStream(tokenSource)
  
  def put(tok : SVToken) = blockingQueue.put(tok)
  def tokenStream = { tokenStream }
}
