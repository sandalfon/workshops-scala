
sealed trait Base {
  def getComplementary(): Base

}

case object A extends Base {
  override def getComplementary() = T
}

case object T extends Base {
  override def getComplementary() = A
}

case object G extends Base {
  override def getComplementary() = C
}

case object C extends Base {
  override def getComplementary() = G
}

object Base {

  /**
    * Try to parse a character as a valid nucleobase
    * Return the corresponding Base if valid (A, T, C, G), else return None
    */
  def get(char: Char): Option[Base] = char match {
    case 'A' => Some(A)
    case 'T' => Some(T)
    case 'G' => Some(G)
    case 'C' => Some(C)
    case _ => None
  }

  /**
    * Parse a character as a valid nucleobase
    *
    * @throws IllegalArgumentException if the `char` parameter doesn't represent a valid nucleobase
    */
  def apply(char: Char): Base = char match {
    case 'A' => A
    case 'T' => T
    case 'G' => G
    case 'C' => C
    case _ => throw new IllegalArgumentException("`" + char + "` parameter doesn't represent a valid nucleobase")
  }


}

type DNA = Seq[Base]

/**
  * Parse a DNA sequence from String as a sequence of nucleobase
  *
  * @throws IllegalArgumentException if the sequence is not valid
  */
def parseDNA(str: String): DNA =
  str.map(char => {
    Base(char)
  })



val dna = parseDNA("AAAATATATGCCCC")

def complementary(dna: DNA): DNA =
  dna.map(base => base.getComplementary())

complementary(dna)

def countBases(dna: DNA): Map[Base, Int] = {
  dna.groupBy(base => base).map(base => (base._1, base._2.length))
}

countBases(dna)


def contains(dna: DNA, subsequence: DNA): Boolean = {


  def helper(subDNA: DNA, subSub: DNA): Boolean = (subDNA, subSub) match {
    case (_, Nil) => true
    case (xs,ys) => {
      if (xs.head == ys.head) helper(xs.tail, ys.tail) else false
    }
  }

  def helper2(dna: DNA, sub: DNA): Boolean = (dna, sub) match {
    case (xs, ys) => if (helper(xs, ys)) true else helper2(xs.tail, ys)
    case _ => {
      println("other");
      false
    }
  }

  helper2(dna, subsequence)
}

val sub =parseDNA("ATAT")
contains(dna, sub)

dna.take(5) ++ parseDNA("CCCC")++dna.drop(5)

def hammingDistance(dna1: DNA, dna2: DNA): Long = {
  def helper(dna1: DNA, dna2:DNA)(acc: Long): Long = (dna1,dna2) match {
    case (Nil,_) =>  acc
    case (xs,ys) => if(xs.head == ys.head) helper(xs.tail,ys.tail)(acc) else helper(xs.tail,ys.tail)(acc+1)
  }
  if(dna1.length< dna2.length) helper(dna1,dna2)(0)
  else
    helper(dna2,dna1)(0)
}


hammingDistance( parseDNA("ATCG"),parseDNA("ATGG"))
hammingDistance( parseDNA("ATCG"),parseDNA("TAGC"))
hammingDistance( parseDNA("TTAAT"),parseDNA("TTAAGCA"))



def basesDifferences(dna1: DNA, dna2: DNA): Seq[Int] = {
  def helper(dna1: DNA, dna2:DNA)(acc: Seq[Int], index: Int): Seq[Int] = (dna1,dna2) match {
    case (Nil,_) =>  acc
    case (xs,ys) => if(xs.head == ys.head) helper(xs.tail,ys.tail)(acc,index +1) else helper(xs.tail,ys.tail)(acc++Seq(index),index+1)
  }
  if(dna1.length< dna2.length) helper(dna1,dna2)(Seq[Int](),0)
  else
    helper(dna2,dna1)(Seq[Int](),0)
}

basesDifferences( parseDNA("ATCG"),parseDNA("ATGG"))
basesDifferences( parseDNA("ATCG"),parseDNA("TAGC"))
basesDifferences( parseDNA("TTAAT"),parseDNA("TTAAGCA"))

//contains(dna, parseDNA("A"))