package patmat

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {

  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  }


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times[a, b, b]") {
    assert(times("abb".toList) === List(('a', 1), ('b', 2)))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("decode and encode [treesbeesbanana] with frenchCode") {
    new TestTrees {
      assert(decode(frenchCode, encode(frenchCode)("treesbeesbanana".toList)) === "treesbeesbanana".toList)
    }
  }

  test("decodedSecret should be huffmanestcool") {
    new TestTrees {
      assert(decodedSecret.mkString === "huffmanestcool")
    }
  }

  test("quickEncode huffmanestcool should be secret") {
    new TestTrees {
      assert(quickEncode(frenchCode)("huffmanestcool".toList) === secret)
    }
  }

  test("createCodeTree should be leaf") {
    new TestTrees {
      val expectedCodeTree = Leaf('a', 1)
      assert(createCodeTree("a".toList) === expectedCodeTree)
    }
  }

  test("createCodeTree should be simple code tree 2 equal elem") {
    new TestTrees {
      val expectedCodeTree = Fork(Leaf('a', 1), Leaf('b', 1), "ab".toList, 2)
      assert(createCodeTree("ab".toList) === expectedCodeTree)
    }
  }

  test("createCodeTree should be simple code tree 2 different elem") {
    new TestTrees {
      val expectedCodeTree = Fork(Leaf('b', 1), Leaf('a', 2), "ba".toList, 3)
      assert(createCodeTree("aab".toList) === expectedCodeTree)
    }
  }

  test("createCodeTree should be simple code tree 3 different elem") {
    new TestTrees {
      val expectedCodeTree = Fork(Leaf('a', 3), Fork(Leaf('c', 1), Leaf('b', 2), "cb".toList, 3), "acb".toList, 6)
      assert(createCodeTree("caaabb".toList) === expectedCodeTree)
    }
  }

  test("createCodeTree should be balanced code tree of 4 different elem") {
    new TestTrees {
      val expectedCodeTree = Fork(
        Fork(Leaf('a', 4), Leaf('b', 5), "ab".toList, 9),
        Fork(Leaf('c', 6), Leaf('d', 7), "cd".toList, 13),
        "abcd".toList,
        22)
      assert(createCodeTree("aaaaccccccdddddddbbbbb".toList) === expectedCodeTree)
    }
  }

  test("createCodeTree should be not balanced code tree of 4 different elem") {
    new TestTrees {
      val expectedCodeTree = Fork(
        Leaf('d', 7),
        Fork(
          Fork(Leaf('a', 1), Leaf('b', 2), "ab".toList, 3),
          Leaf('c', 6),
          "abc".toList,
          9),
        "dabc".toList,
        16)
      assert(createCodeTree("accccccdddddddbb".toList) === expectedCodeTree)
    }
  }

  test("verify codeBits") {
    new TestTrees {
      val orderedLeafList: List[Leaf] =
        Leaf('\n', 1) ::
          Leaf('s', 3) ::
          Leaf('t', 4) ::
          Leaf('a', 10) ::
          Leaf('i', 12) ::
          Leaf('p', 13) ::
          Leaf('e', 15) ::
          Nil
      val codeTable      : CodeTable  = convert(until(singleton, combine)(orderedLeafList).head)

      assert(codeBits(codeTable)('a').mkString === "111")
      assert(codeBits(codeTable)('e').mkString === "10")
      assert(codeBits(codeTable)('i').mkString === "00")
      assert(codeBits(codeTable)('s').mkString === "11011")
      assert(codeBits(codeTable)('t').mkString === "1100")
      assert(codeBits(codeTable)('p').mkString === "01")
      assert(codeBits(codeTable)('\n').mkString === "11010")
    }
  }

}
