import Huffman.Encoder


object Program {
  def main(args: Array[String]): Unit = {
    val freqMap = Map(('c' -> 1), ('a' -> 2), ('t' -> 3), ('s' -> 4))
    val encoder = Encoder.create(freqMap)
    
    val encoded = encoder("caatttssss");
    println(encoded)
  }
}