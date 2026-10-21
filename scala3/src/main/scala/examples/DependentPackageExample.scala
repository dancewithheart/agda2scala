package examples

import examples.DependentPackage.Package

val p: Package { type Carrier = Long } =
  new Package:
    type Carrier = Long
    def item: Carrier = 42L

val value: p.Carrier = p.item