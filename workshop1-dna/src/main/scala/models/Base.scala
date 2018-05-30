package models

sealed trait Base {
  def getComplementary() : Base

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
  def get(char: Char): Option[Base] = char match{
    case 'A' => Some(A)
    case 'T' => Some(T)
    case 'G' => Some(G)
    case 'C' => Some(C)
    case _ => None
  }

  /**
    * Parse a character as a valid nucleobase
    * @throws IllegalArgumentException if the `char` parameter doesn't represent a valid nucleobase
    */
  def apply(char: Char): Base = char match{
    case 'A' => A
    case 'T' => T
    case 'G' => G
    case 'C' => C
    case _ => throw  new IllegalArgumentException("`"+char+"` parameter doesn't represent a valid nucleobase")

  }



}
