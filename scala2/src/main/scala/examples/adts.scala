package examples

object adts
{

sealed trait Rgb
object Rgb {
  case object Red extends Rgb
  case object Green extends Rgb
  case object Blue extends Rgb

}

sealed trait Bool
object Bool {
  case object True extends Bool
  case object False extends Bool

}

sealed trait Color
object Color {
  final case class Light(x0: Rgb) extends Color
  final case class Dark(x0: Rgb) extends Color

}

final case class RgbPair(fst: Rgb, snd: Bool)

def idRgb(x0: Rgb): Rgb = x0

def rgbConstTrue1(rgb: Rgb): Bool = Bool.True

def constRgbPair(rgbPairArg: RgbPair, rgbArg: Rgb): RgbPair = rgbPairArg

def hello(): String = "Hello, world!"

def withEscapes(): String = "line1\nline2\t\"quote\"\\backslash"

def id[A](x1: A): A = x1
}
