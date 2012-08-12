package com.derekschaefer.classifier

import org.scalatest.FunSuite

class TokenizerTest extends FunSuite {

  test("eachWord should remove stop words.") {
    val t = new Tokenizer
    val text = "How's it going? Having a good time?"
    val result = List("going", "good", "time")
    assert(t.eachWord(text) === result)
  }

  test("eachWord should remove stop words and perform stemming.") {
    val t = new Tokenizer("english")
    val text = "Welcome to the Internet, friend! Enjoy your stay."
    val result = List("welcom", "internet", "friend", "enjoy", "stay")
    assert(t.eachWord(text) === result)
  }

}
