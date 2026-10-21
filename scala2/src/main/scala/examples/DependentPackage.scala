package examples

object DependentPackage
{

trait Package {
  type Carrier
  def item: Carrier
}
}
