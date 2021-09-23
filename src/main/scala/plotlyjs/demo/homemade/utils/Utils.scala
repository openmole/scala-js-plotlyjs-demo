package plotlyjs.demo.homemade.utils

import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.openmole.plotlyjs.{PlotData, Plotly}
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

}
