package examples

object adts:
  enum Rgb:
    case Red
    case Green
    case Blue

  enum Bool:
    case True
    case False

  final case class RgbPair(snd: Bool, fst: Rgb)

  def idRgb(theArg: Rgb): Rgb = theArg

  def constRgbPair(rgbArg: Rgb, rgbPairArg: RgbPair): RgbPair = rgbPairArg
