package examples

object DependentPackage:
  trait Package:
    type Carrier
    def item: Carrier

  def example(): Package = new Package:
    type Carrier = Long
    def item: Carrier = 42
