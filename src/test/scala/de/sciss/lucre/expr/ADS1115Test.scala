package de.sciss.lucre.expr

import de.sciss.lucre.edit.UndoManager
import de.sciss.lucre.synth.InMemory
import de.sciss.proc.{ExprContext, Universe}

object ADS1115Test {
  def main(args: Array[String]): Unit = {
    type S = InMemory
    type T = InMemory.Txn

    val g = Graph {
      import graph._

      val ads = GPIO.ADS1X15()
      val run = ads.runSingle(0)
      val in  = ads.in(0)

      val b = LoadBang()
      b --> Act(
        PrintLn("Running ADC"),
        run
      )
      run --> PrintLn("The value is %d".format(in))
    }

    implicit val system: S = InMemory()

    system.step { implicit tx =>
      implicit val u    : Universe    [T] = Universe.dummy
      implicit val undo : UndoManager [T] = UndoManager()
      implicit val ctx  : Context     [T] = ExprContext()
      g.expand[T].initControl()
    }

  }
}
