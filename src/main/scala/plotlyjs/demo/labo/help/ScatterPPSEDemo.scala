package plotlyjs.demo.labo.help

import com.raquo.laminar.api.L._
import org.openmole.plotlyjs.all.scatter
import org.openmole.plotlyjs.{Layout, Plotly}
import plotlyjs.demo.demo.Demo

import scala.scalajs.js
import scala.scalajs.js.JSConverters.JSRichIterableOnce

/*
 * Copyright (C) 24/03/16 // mathieu.leclaire@openmole.org
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 */
object ScatterPPSEDemo {

  private lazy val sc = sourcecode.Text {

    val plotDiv = div()

    val points = SquareDiagonalTransformation.vectors(64)
    val coords = points.transpose

    val data = scatter
      .x(coords(0).toJSArray)
      .y(coords(1).toJSArray)

    val layout = Layout
      .width(1024)
      .height(1024)
    Plotly.plot(plotDiv.ref, js.Array(data), layout._result)

    plotDiv
  }

  val elementDemo: Demo = new Demo {
    override def isLazy: Boolean = true

    def title: String = "Scatter for PPSE"

    def code: String = sc.source

    def element: HtmlElement = sc.value
  }

}
