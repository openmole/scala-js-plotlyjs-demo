package plotlyjs.demo.demo

import com.raquo.laminar.api.L._

object ParetoAPIDemo {

  private lazy val sc = sourcecode.Text {
    import plotlyjs.demo.homemade.api.Data.{Input, Outcome, Output}
    import plotlyjs.demo.homemade.api.Pareto.{Minimization, ParetoDisplay, ParetoObjective, pareto}
    import plotlyjs.demo.homemade.utils.ParetoFrontGenerator

    val dimension = 5
    val objectives = (0 until dimension).map(i => ParetoObjective("objective n°" + (i + 1), Minimization))
    val outcomes = ParetoFrontGenerator.random(dimension, 42).map(v => Outcome(
      (0 until dimension).map(i => Input("", i)),
      v.map(c => Output("", c))
    ))
    div(
      pareto(
        objectives,
        outcomes,
        ParetoDisplay(outputPath = false)
      ),
      pareto(
        objectives,
        outcomes,
        ParetoDisplay(outputPath = false, lowerIsBetter = true) // 180° rotation
      )
    )
  }

  val elementDemo: Demo = new Demo {
    def title: String = "Pareto API"
    def code: String = sc.source
    def element: HtmlElement = sc.value
  }

}
