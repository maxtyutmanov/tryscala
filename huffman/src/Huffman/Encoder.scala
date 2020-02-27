package Huffman
 
sealed class Node(val char: Char, val freq: Int)

object Encoder {
  def create(freqMap: Map[Char,Int]): Encoder = {
    val freqArray = freqMap.toSeq.sortWith(_._2 > _._2)
    val ht1 = freqArray.map(x => new Node(x._1, x._2)).foldLeft(Nil: Tree[Node])((t, n) => insert(t, n))
    val ht2 = Tree.mapPath(ht1)((n, path) => (n, pathToStr(path)))
    
    val encMap = Tree.fold(ht2)(leaf => Map(leaf._1.char -> leaf._2))((lmap, rmap) => lmap)
    
    (source: String) => 
      String.join(source.map(c => encMap(c)))
    
    //val codeMap = Tree.fold(huffmanTree)(leaf => new Map[Char, )((l, r) =>  
//    (source: String) => {
//      
//    }
  }
  
  private def insert(t: Tree[Node], newN: Node): Tree[Node] = t match {
    case Leaf(n) => Branch(t, Leaf(newN))
    case Branch(l, r) => insert(t, newN)
    case Nil => Leaf(newN)
  }
  
  private def pathToStr(path: List[Turn]): String = {
    val sb = new StringBuilder()
    path.foldRight(sb)((turn, builder) => turn match {
      case Left => sb.append('0')
      case Right => sb.append('1')
    }).toString()
  }
}