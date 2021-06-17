package plotlyjs.demo

import com.raquo.laminar.api.L._
import org.openmole.plotlyjs.PlotMode.markers
import org.openmole.plotlyjs.PlotlyImplicits._
import org.openmole.plotlyjs._
import org.openmole.plotlyjs.all._
import org.openmole.plotlyjs.plotlyConts._
import plotlyjs.demo.{Data, ElementDemo}
import tools.AngularAdjustment.angularAdjustment
import tools.Vectors

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
object AngularAdjustmentTest {

  private val sc = sourcecode.Text {

    def scatter3dData(name: String, points: Seq[Seq[Double]], color: Color) = {
      val pointsT = points.transpose
      scatter3d
        .name(name)
        .x(pointsT(0).toJSArray)
        .y(pointsT(1).toJSArray)
        .z(pointsT(2).toJSArray)
        .setMode(markers)
        .marker(marker
          .size(1)
          .symbol(circle)
          .color(color)
        )._result
    }

    def scatter3dDiv(cubicPoints: Seq[Seq[Double]], color: Color) = {
      val plotDiv = div()

      val plotDataSeq = Seq(
        scatter3dData("cubic", cubicPoints, color),
        scatter3dData("spherical", cubicPoints.map(Vectors.normalize), color),
      )

      Plotly.plot(plotDiv.ref, plotDataSeq.toJSArray)

      plotDiv
    }

    val highCorner = Data.highCorner(3, 16)
    val adjustedHighCorner = highCorner.map(angularAdjustment)

   div(
      scatter3dDiv(highCorner, Color.rgb(255, 0, 0)),
      scatter3dDiv(adjustedHighCorner, Color.rgb(0, 0, 255)),
    )
  }

  val elementDemo: ElementDemo = new ElementDemo {
    def title: String = "AngularAdjustment"
    def code: String = sc.source
    def element: HtmlElement = sc.value
  }

}
