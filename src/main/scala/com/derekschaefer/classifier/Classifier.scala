package com.derekschaefer.classifier

import collection.mutable.HashMap

class Classifier(val _lang: String) {

  def this() = this("english")

  private val _tk = new Tokenizer(_lang)

  private val _wcount = HashMap[Pair[String, String], Int]()
  private val _ccount = HashMap[String, Int]()
  private val _probs  = HashMap[String, Double]()

  def wcount() = _wcount.toMap
  def ccount() = _ccount.toMap

  def train(category: String, text: String) {
    _ccount.put(category, 1 + _ccount.get(category).getOrElse(0))
    _tk.eachWord(text).foreach(w =>
      _wcount.put((category, w), 1 + _wcount.get((category, w)).getOrElse(0))
    )
  }

  def classify(text: String): String = {
    categoryScores(text)
    _probs.maxBy(_._2)._1
  }

  private def totalCount(): Int = _ccount.values.sum

  private def categories(): List[String] = _ccount.keys.toList

  private def textProb(category: String, text: String): Double = {
    _ccount.get(category).getOrElse(0).toDouble / totalCount.toDouble * documentProb(category, text)
  }

  private def documentProb(category: String, text: String): Double = {
    var prob = 1.0
    _tk.eachWord(text).foreach(prob *= wordWeightedAvg(category, _))
    prob
  }

  private def categoryScores(text: String) {
    _ccount.foreach(c => _probs.put(c._1, textProb(c._1, text)))
  }

  private def wordProb(category: String, word: String): Double = {
    _wcount.get((category, word)) match {
      case Some(c) => c.toDouble / _ccount.get(category).getOrElse(1).toDouble
      case None    => 0.0
    }
  }

  private def wordWeightedAvg(category: String, word: String): Double = {
    val weight = 1.0
    val assumed_prob = 0.5
    val basic_prob = wordProb(category, word)
    var totals = 0
    _ccount.foreach(cat =>
      _wcount.get(cat._1, word) match {
        case Some(v) => totals += v
        case None    =>
      }
    )
    (weight * assumed_prob + totals * basic_prob) / (weight + totals)
  }

}
