package examples

object adts
{

sealed trait Rgb
object Rgb {
  case object Red extends Rgb
  case object Green extends Rgb
  case object Blue extends Rgb

}

sealed trait Answer
object Answer {
  case object Yes extends Answer
  case object No extends Answer

}

sealed trait Color
object Color {
  final case class Light(x0: Rgb) extends Color
  final case class Dark(x0: Rgb) extends Color

}

final case class RgbPair(fst: Rgb, snd: Answer)

def idRgb(x0: Rgb): Rgb = x0

def rgbConstYes1(rgb: Rgb): Answer = Answer.Yes

def constRgbPair(rgbPairArg: RgbPair, rgbArg: Rgb): RgbPair = rgbPairArg

def hello(): String = "Hello, world!"

def withEscapes(): String = "line1\nline2\t\"quote\"\\backslash"

def id[A](x1: A): A = x1

sealed trait Maybe[A]
object Maybe {
  final case class Just[A](x0: A) extends Maybe[A]
  case object None extends Maybe[Nothing]

}

sealed trait List[X]
object List {
  case object Nil extends List[Nothing]
  final case class Cons[X](x0: X, x1: List[X]) extends List[X]

}
}
