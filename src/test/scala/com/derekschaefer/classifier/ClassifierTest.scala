package com.derekschaefer.classifier

import org.scalatest.FunSuite

class ClassifierTest extends FunSuite {

  test("wcount should contain the number of word occurrences.") {
    val cls = new Classifier
    cls.train("test", "The man in black fled across the desert, and the Gunslinger followed.")
    val wcount = Map(
      ("test", "fled") -> 1,
      ("test", "follow") -> 1,
      ("test", "man") -> 1,
      ("test", "gunsling") -> 1,
      ("test", "black") -> 1,
      ("test", "desert") -> 1,
      ("test", "across") -> 1
    )
    assert(cls.wcount === wcount)
  }

  test("ccount should contain the number of category occurrences.") {
    val cls = new Classifier
    cls.train("test", "This is some text.")
    val ccount1 = Map(("test") -> 1)
    assert(cls.ccount === ccount1)
    cls.train("test", "This is some more text.")
    val ccount2 = Map(("test") -> 2)
    assert(cls.ccount === ccount2)
  }

  test("classify should correctly categorize text.") {
    val classifier = new Classifier
    classifier.train("positive", "good stuff")
    classifier.train("negative", "bad stuff")
    assert(classifier.classify("This test is really good stuff.") === "positive")
    assert(classifier.classify("This test is bad news, man.") === "negative")
  }

}
