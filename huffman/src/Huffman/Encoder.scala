package Huffman
 
sealed class Node(val char: Char, val freq: Int)

sealed class ListNode(val chars: List[Char], val freq: Int, val subtree: Tree[Node]) {
  def merge(another: ListNode): ListNode = {
    val newChars = List.concat(chars, another.chars)
    val newFreq = freq + another.freq
    val newSubtree = Branch[Node](subtree, another.subtree)
    
    new ListNode(newChars, newFreq, newSubtree)
  }
}

object Encoder {
  def create(freqMap: Map[Char,Int]): Encoder = {
    val freqArray = sortFreqMapByDesc(freqMap)
    val ht1 = buildHuffmanTree(freqArray)
    val ht2 = Tree.mapPath(ht1)((n, path) => (n, pathToStr(path)))
    
    val encMap = Tree.fold(ht2)(leaf => Map(leaf._1.char -> leaf._2))((lmap, rmap) => mergeMaps(lmap, rmap))
    
    (source: String) => {
      val strs = source.map(c => encMap(c))
      strs.mkString("")
    }
  }
  
  private def buildHt2(es: List[ListNode]): List[ListNode] = es match {
    case scala.Nil => scala.Nil
    case h1::h2::t => h1.merge(h2)::buildHt2(t)
    case h1::scala.Nil => es
  }
  
  private def sortedInsert(es: List[ListNode], n: ListNode, gt: (ListNode, ListNode) => Boolean): List[ListNode] = {
    val (before, after) = es span (i => gt(n, i))
    List.concat(before, n::after)
  }
    
  //private def sortedInsert(
  
  private def buildHuffmanTree(freq: Seq[(Char,Int)]): Tree[Node] =
    freq.map(x => new Node(x._1, x._2)).foldLeft(Nil: Tree[Node])((t, n) => insert(t, n))
  
  private def sortFreqMapByDesc(freqMap: Map[Char,Int]) : Seq[(Char,Int)] = {
    freqMap.toSeq.sortWith(_._2 > _._2)
  }
  
  private def insert(t: Tree[Node], newN: Node): Tree[Node] = t match {
    case Leaf(n) => Branch(t, Leaf(newN))
    case Branch(l, r) => Branch(l, insert(r, newN))
    case Nil => Leaf(newN)
  }
  
  def mergeMaps[K, V](m1:Map[K, V], m2:Map[K, V]):Map[K, V] = 
    (m1.keySet ++ m2.keySet) map { k => k -> (m1.get(k).toList ::: m2.get(k).toList).head } toMap
  
  private def pathToStr(path: List[Turn]): String = {
    val sb = new StringBuilder()
    path.foldRight(sb)((turn, builder) => turn match {
      case Left => sb.append('0')
      case Right => sb.append('1')
    }).toString()
  }
}