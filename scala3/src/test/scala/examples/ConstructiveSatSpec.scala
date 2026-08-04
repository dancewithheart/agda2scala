package examples

import zio.test.*

import examples.ConstructiveSat
import examples.ConstructiveSat.{Environment as SatEnv}
import examples.ConstructiveSat.Formula
import examples.ConstructiveSat.Variable

object ConstructiveSatSpec extends ZIOSpecDefault:
  private val environments: List[SatEnv] = ConstructiveSat.allEnvironments()
  private val variableGen: Gen[Any, Variable] = Gen.fromIterable(List(Variable.p, Variable.q, Variable.r))

  private def formulaGen(depth: Int): Gen[Any, Formula] =
    if depth <= 0 then
      variableGen.map(variable => Formula.atom(variable))
    else {
      val smaller = formulaGen(depth - 1)

      Gen.oneOf(
        variableGen.map(variable => Formula.atom(variable)),
        smaller.map(formula => Formula.neg(formula)),
        smaller.zip(smaller).map { case (left, right) =>
          Formula.conj(left, right)
        },
        smaller.zip(smaller).map { case (left, right) =>
          Formula.disj(left, right)
        },
        smaller.zip(smaller).map { case (left, right) =>
          Formula.impl(left, right)
        }
      )
    }

  def spec =
    suite("Constructive SAT")(
      test("enumerates all eight environments") {
        assertTrue(
          environments.size == 8,
          environments.distinct.size == 8
        )
      },

      test("solves the documented satisfiable formula") {
        val formula = ConstructiveSat.exampleSat()
        val solution = ConstructiveSat.solve(formula)

        assertTrue(
          solution.isDefined,
          solution.exists(environment =>
            ConstructiveSat.evaluate(environment, formula)
          ),
          solution.exists(environment =>
            environment.pValue && !environment.qValue
          )
        )
      },

      test("rejects the documented contradiction") {
        val formula = ConstructiveSat.exampleUnsat()

        assertTrue(
          ConstructiveSat.solve(formula).isEmpty,
          !environments.exists(environment =>
            ConstructiveSat.evaluate(environment, formula)
          )
        )
      },

      test("can construct and solve a formula from generated Scala API") {
        val formula =
          Formula.impl(
            Formula.conj(
              Formula.atom(Variable.p),
              Formula.atom(Variable.q)
            ),
            Formula.atom(Variable.p)
          )

        val result = ConstructiveSat.solve(formula)

        assertTrue(
          result.isDefined,
          result.exists(environment =>
            ConstructiveSat.evaluate(environment, formula)
          )
        )
      },

      test("generated Boolean operations agree with Scala Boolean operations") {
        check(Gen.boolean.zip(Gen.boolean)) { case (left, right) =>
          assertTrue(
            ConstructiveSat.not(left) == !left,
            ConstructiveSat.and(left, right) == (left && right),
            ConstructiveSat.or(left, right) == (left || right),
            ConstructiveSat.implies(left, right) == (!left || right)
          )
        }
      },

      test("solver agrees with direct exhaustive search") {
        check(formulaGen(depth = 4)) { formula =>
          val certifiedResult = ConstructiveSat.solve(formula)
          val directResult =environments.find(environment =>
            ConstructiveSat.evaluate(environment, formula)
          )

          assertTrue(
            certifiedResult == directResult
          )
        }
      },

      test("every returned environment satisfies its formula") {
        check(formulaGen(depth = 4)) { formula =>
          val result = ConstructiveSat.solve(formula)

          assertTrue(
            result.forall(environment =>
              ConstructiveSat.evaluate(environment, formula)
            )
          )
        }
      }
    )
