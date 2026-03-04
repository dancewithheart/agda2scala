package examples

object adts:
  enum Rgb:
    case Red
    case Green
    case Blue

  enum Bool:
    case True
    case False

  enum Color:
    case Light(x0: Rgb)
    case Dark(x0: Rgb)

  final case class RgbPair(fst: Rgb, snd: Bool)

  def idRgb(x0: Rgb): Rgb = x0

  def rgbConstTrue1(rgb: Rgb): Bool = Bool.True

  def constRgbPair(rgbPairArg: RgbPair, rgbArg: Rgb): RgbPair = rgbPairArg

  def hello(): String = "Hello, world!"

  def withEscapes(): String = "line1\nline2\t\"quote\"\\backslash"

  def id[A](x1: A): A = x1
