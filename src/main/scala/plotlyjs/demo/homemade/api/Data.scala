package plotlyjs.demo.homemade.api

object Data {

  case class Input(name: String, value: Double)

  case class Output(name: String, value: Double)

  case class Outcome(inputs: Seq[Input], outputs: Seq[Output], samples: Option[Int] = None)

}
