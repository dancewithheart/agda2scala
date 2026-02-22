package examples

object adts:
  enum Rgb:
    case Red
    case Green
    case Blue

  enum Bool:
    case True
    case False

  final case class RgbPair(fst: Rgb, snd: Bool)

  def idRgb(x0: Rgb): Rgb = x0

  def constRgbPair(rgbPairArg: RgbPair, rgbArg: Rgb): RgbPair = rgbPairArg
