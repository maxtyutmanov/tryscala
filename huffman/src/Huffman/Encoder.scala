package Huffman
 
sealed case class HTreeNode(val char: Char, val freq: Int)

sealed class HListNode(val chars: List[Char], val freq: Int, val subtree: Tree[HTreeNode]) {
  def merge(another: HListNode): HListNode = {
    val newChars = scala.List.concat(chars, another.chars)
    val newFreq = freq + another.freq
    val newSubtree = Branch[HTreeNode](subtree, another.subtree)
    
    new HListNode(newChars, newFreq, newSubtree)
  }
}

object Encoder {
  def create(freqMap: Map[Char,Int]): Encoder = {
    val freqArray = sortFreqMapByDesc(freqMap)
    val ht1 = buildHuffmanTree(freqMap)
    val ht2 = Tree.mapPath(ht1)((n, path) => (n, pathToStr(path)))
    
    val encMap = Tree.fold(ht2)(leaf => Map(leaf._1.char -> leaf._2))((lmap, rmap) => mergeMaps(lmap, rmap))
    
    (source: String) => {
      val strs = source.map(c => encMap(c))
      strs.mkString("")
    }
  }
  
  private def buildHuffmanTree(freqMap: Map[Char,Int]): Tree[HTreeNode] = {
    val freqArray = sortFreqMapByDesc(freqMap)
    val initialTable = freqArray.map(x => new HListNode(scala.List(x._1), x._2, Leaf(HTreeNode(x._1, x._2)))).toList
    mergeTable(initialTable).head.subtree
  }
  
  private def mergeTable(es: List[HListNode]): List[HListNode] = es match {
    case scala.Nil => scala.Nil
    case h1::h2::t => mergeTable(List.sortedInsert[HListNode](t, h1.merge(h2), (ln1, ln2) => ln1.freq > ln2.freq))
    case h1::scala.Nil => es
  }
  
  private def sortFreqMapByDesc(freqMap: Map[Char,Int]) : Seq[(Char,Int)] = {
    freqMap.toSeq.sortWith(_._2 < _._2)
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

object List {
  def sortedInsert[A](es: List[A], n: A, gt: (A, A) => Boolean): List[A] = {
    val (before, after) = es span (i => gt(n, i))
    scala.List.concat(before, n::after)
  }
}