package com.derekschaefer.classifier

object Main {

  def read(f: (String) => Unit) {
    Iterator.continually(Console.readLine).takeWhile(_ != "").foreach(f)
  }

  def main(args: Array[String]) {
    val cls = new Classifier
    val Train = """([^:]+):(.+)""".r

    println("Enter training data (i.e. category:text)\n")

    read(l => {
      l match {
        case Train(c, t) => {
          println("Training: " + c + " -> " + t)
          cls.train(c, t)
        }
        case _ => println("Invalid format, please try again.\n")
      }
    })

    println("\nNow classify some strings\n")

    read(l => println(l + " = " + cls.classify(l)))

    println("\nDone.")
  }

}
