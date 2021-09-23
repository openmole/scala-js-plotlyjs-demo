package plotlyjs.demo.homemade.utils

import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.openmole.plotlyjs.{Icon, Layout, ModeBarButton, PlotData, Plotly}
import org.scalajs.dom.html
import plotlyjs.demo.homemade.utils.Utils.ExtraTraceManager.ExtraTracesRef
import plotlyjs.demo.homemade.utils.Vectors._
import plotlyjs.demo.utils.Utils.printCode

import scala.math.{ceil, random}
import scala.scalajs.js.JSConverters._

object Utils {

  def randomizeDimensions(seq: Seq[Vector]): Seq[Vector] = {
    seq.headOption.map(head => {
      val dimension = head.dimension
      val mulVector = (() => ceil(10 * random)) at dimension
      val addVector = (() => 10 * random - 5) at dimension
      seq
        .map(_.mul(mulVector))
        .map(_.add(addVector))
    }).getOrElse(Seq[Vector]())
  }

  class SkipOnBusy {
    private var busy = false
    def skipOnBusy(name: String, f: () => Unit): Unit = {
      if(!busy) {
        busy = true
        //println(name + "...")
        f()
        //println(name + ".")
        busy = false
      } else {
        //println(name + " skipped")
      }
    }
  }

  class ExtraTraceManager(plotDiv: ReactiveHtmlElement[html.Div], initialTraceCount: Int) {

    private var extraSize = 0
    private var refs = Seq[ExtraTracesRef]()

    private def removeRef(ref: ExtraTracesRef): Unit = {
      ref.isNull = true
      refs = refs.filterNot(_.isNull)
    }

    private def addRef(ref: ExtraTracesRef): Unit = {
      removeRef(ref)
      ref.isNull = false
      refs = refs :+ ref
    }

    private def clearRefs(): Unit = {
      refs.foreach(_.isNull = true)
      refs = Seq[ExtraTracesRef]()
    }

    def deleteTraces(ref: ExtraTracesRef): Unit = {
      if(!ref.isNull) {
        val size = ref.size

        Plotly.deleteTraces(
          plotDiv.ref,
          (ref.from until ref.to)
            .map(_ + initialTraceCount)
            .map(_.toDouble)
            .toJSArray
        )
        extraSize -= size

        removeRef(ref)
        refs
          .filter(ref.to <= _.from)
          .foreach { nextRef =>
            nextRef.from -= size
            nextRef.to -= size
          }
      }
    }

    def updateTraces(ref: ExtraTracesRef, optionDataSeq: Option[Seq[PlotData]]): Unit = {
      deleteTraces(ref)
      optionDataSeq match {
        case Some(plotDataSeq) => {
          ref.from = extraSize
          ref.to = extraSize + plotDataSeq.size

          Plotly.addTraces(plotDiv.ref, plotDataSeq.map(Option(_).orUndefined).toJSArray)
          extraSize = ref.to

          addRef(ref)
        }
        case _ =>
      }
    }

    def deleteAllTraces(): Unit = {
      Plotly.deleteTraces(
        plotDiv.ref,
        (0 until extraSize)
          .map(_ + initialTraceCount)
          .map(_.toDouble)
          .toJSArray
      )
      extraSize = 0

      clearRefs()
    }

  }

  object ExtraTraceManager {

    //need some access restriction for vars
    class ExtraTracesRef(var from: Int, var to: Int) {
      var isNull: Boolean = false
      def size: Int = to - from
    }

    def nullRef: ExtraTracesRef = {
      val ref = new ExtraTracesRef(-1, -1)
      ref.isNull = true
      ref
    }

  }

  def resetViewButton(plotDiv: ReactiveHtmlElement[html.Div], layout: Layout): ModeBarButton = {
    ModeBarButton
      .name("Reset view")
      .icon(Icon // plotly home icon
        .width(928.6)
        .height(1000)
        .path("m786 296v-267q0-15-11-26t-25-10h-214v214h-143v-214h-214q-15 0-25 10t-11 26v267q0 1 0 2t0 2l321 264 321-264q1-1 1-4z m124 39l-34-41q-5-5-12-6h-2q-7 0-12 3l-386 322-386-322q-7-4-13-4-7 2-12 7l-35 41q-4 5-3 13t6 12l401 334q18 15 42 15t43-15l136-114v109q0 8 5 13t13 5h107q8 0 13-5t5-13v-227l122-102q5-5 6-12t-4-13z")
        .transform("matrix(1 0 0 -1 0 850)")
      )
      .click(() => Plotly.relayout(plotDiv.ref, layout))
      ._result
  }

}
