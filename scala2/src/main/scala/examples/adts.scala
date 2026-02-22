package examples

object adts {

sealed trait Rgb
case object Red extends Rgb
case object Green extends Rgb
case object Blue extends Rgb

sealed trait Bool
case object True extends Bool
case object False extends Bool

final case class RgbPair(snd: Bool, fst: Rgb)

def idRgb(x0: Rgb): Rgb = theArg

def constRgbPair(rgbPairArg: RgbPair, rgbArg: Rgb): RgbPair = rgbPairArg
}
