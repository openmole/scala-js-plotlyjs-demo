package plotlyjs.demo.demo

import com.raquo.laminar.api.L._

import scala.math.{random, rint}

object ParetoAPIDemo {

  private lazy val sc = sourcecode.Text {
    import plotlyjs.demo.homemade.api.Data.{Input, Outcome, Output}
    import plotlyjs.demo.homemade.api.Pareto.{Minimization, ParetoDisplay, ParetoObjective, pareto}
    import plotlyjs.demo.homemade.utils.ParetoFrontGenerator

    val dimension = 5
    val objectives = (0 until dimension).map(i => ParetoObjective("objective " + (i + 1), Minimization))
    val outcomes = ParetoFrontGenerator.random(dimension, 42).map(v => Outcome(
      (0 until dimension).map(i => Input("", i)),
      v.map(c => Output("", c)),
      Some(rint(random() * 100).toInt)
    ))

    div(
      pareto(
        objectives,
        outcomes,
        ParetoDisplay(800)
      ),
      pareto(
        objectives,
        outcomes,
        ParetoDisplay(800, showPath = true)
      )
    )
  }

  val elementDemo: Demo = new Demo {
    def title: String = "Pareto API"
    def code: String = sc.source
    def element: HtmlElement = sc.value
  }

}
