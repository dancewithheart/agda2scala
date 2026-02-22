package examples

object adts {

sealed trait Rgb
case object Red extends Rgb
case object Green extends Rgb
case object Blue extends Rgb

sealed trait Bool
case object True extends Bool
case object False extends Bool

final case class RgbPair(fst: Rgb, snd: Bool)

def idRgb(x0: Rgb): Rgb = x0

def rgbConstTrue1(rgb: Rgb): Bool = True

def constRgbPair(rgbPairArg: RgbPair, rgbArg: Rgb): RgbPair = rgbPairArg

def hello(): String = "hi"

def two(): Nat = 2
}
