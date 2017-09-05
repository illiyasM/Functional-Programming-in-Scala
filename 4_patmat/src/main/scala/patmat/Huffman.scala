package patmat

//import common._

object Huffman {

  abstract class CodeTree

  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree

  case class Leaf(char: Char, weight: Int) extends CodeTree

  // Part 1: Basics
  def weight(tree: CodeTree): Int = tree match {
    case Fork(_, _, _, x) => x
    case Leaf(_, x) => x
  }

  def chars(tree: CodeTree): List[Char] = tree match {
    case Fork(_, _, x, _) => x
    case Leaf(x, _) => List(x)
  }

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))

  // Part 2: Generating Huffman trees
  def string2Chars(str: String): List[Char] = str.toList

  def times(chars: List[Char]): List[(Char, Int)] = chars.groupBy(x => x).map(xs => (xs._1, xs._2.length)).toList

  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = freqs.sortWith((f1, f2) => f1._2 < f2._2).map(f => Leaf(f._1, f._2))

  def singleton(trees: List[CodeTree]): Boolean = trees.length == 1

  def combine(trees: List[CodeTree]): List[CodeTree] = trees match {
    case elem1 :: elem2 :: elem => (makeCodeTree(elem1, elem2) :: elem).sortBy(x => weight(x))
    case _ => trees
  }

  def until(s: List[CodeTree] => Boolean, c: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): List[CodeTree] = {
    if (s(trees)) trees
    else until(s, c)(c(trees))
  }

  def createCodeTree(chars: List[Char]): CodeTree = until(singleton, combine)(makeOrderedLeafList(times(chars))).head

  // Part 3: Decoding
  type Bit = Int

  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
    def decodeHelp(tree: CodeTree, bits: List[Bit]): (Char, List[Bit]) = (tree, bits) match {
      case (Leaf(m, _), xs) => (m, xs)
      case (Fork(m, n, _, _), x :: xs) => decodeHelp(if (x == 0) m else n, xs)
      case (_, Nil) => sys.error("Input.Error")
      case (_, _) => sys.error("Input.error")
    }
    if (bits.isEmpty) Nil else {
      val (carac, bitTail) = decodeHelp(tree, bits)
      carac :: decode(tree, bitTail)
    }
  }

  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s', 121895), Fork(Leaf('d', 56269), Fork(Fork(Fork(Leaf('x', 5928), Leaf('j', 8351), List('x', 'j'), 14279), Leaf('f', 16351), List('x', 'j', 'f'), 30630), Fork(Fork(Fork(Fork(Leaf('z', 2093), Fork(Leaf('k', 745), Leaf('w', 1747), List('k', 'w'), 2492), List('z', 'k', 'w'), 4585), Leaf('y', 4725), List('z', 'k', 'w', 'y'), 9310), Leaf('h', 11298), List('z', 'k', 'w', 'y', 'h'), 20608), Leaf('q', 20889), List('z', 'k', 'w', 'y', 'h', 'q'), 41497), List('x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 72127), List('d', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 128396), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 250291), Fork(Fork(Leaf('o', 82762), Leaf('l', 83668), List('o', 'l'), 166430), Fork(Fork(Leaf('m', 45521), Leaf('p', 46335), List('m', 'p'), 91856), Leaf('u', 96785), List('m', 'p', 'u'), 188641), List('o', 'l', 'm', 'p', 'u'), 355071), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u'), 605362), Fork(Fork(Fork(Leaf('r', 100500), Fork(Leaf('c', 50003), Fork(Leaf('v', 24975), Fork(Leaf('g', 13288), Leaf('b', 13822), List('g', 'b'), 27110), List('v', 'g', 'b'), 52085), List('c', 'v', 'g', 'b'), 102088), List('r', 'c', 'v', 'g', 'b'), 202588), Fork(Leaf('n', 108812), Leaf('t', 111103), List('n', 't'), 219915), List('r', 'c', 'v', 'g', 'b', 'n', 't'), 422503), Fork(Leaf('e', 225947), Fork(Leaf('i', 115465), Leaf('a', 117110), List('i', 'a'), 232575), List('e', 'i', 'a'), 458522), List('r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 881025), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u', 'r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 1486387)
  val secret: List[Bit] = List(0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1)

  /**
    * Write a function that returns the decoded secret
    */
  def decodedSecret: List[Char] = decode(frenchCode, secret)

  // Part 4a: Encoding using Huffman tree
  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    def helperFunc(tree: CodeTree)(carac: Char): List[Bit] = tree match {
      case Leaf(_, _) => Nil
      case Fork(x, y, _, _) => if (chars(x).contains(carac)) 0::helperFunc(x)(carac) else 1::helperFunc(y)(carac)
    }
    text.flatMap(helperFunc(tree))
  }

  // Part 4b: Encoding using code table
  type CodeTable = List[(Char, List[Bit])]

  def codeBits(table: CodeTable)(char: Char): List[Bit] = table.filter(x => x._1 == char).head._2

  def convert(tree: CodeTree): CodeTable = tree match {
    case Fork(x, y, _, _) => mergeCodeTables(convert(x), convert(y))
    case Leaf(x, _) => List((x, List()))
  }

  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = a ::: b

  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = text.flatMap(codeBits(convert(tree)))
}