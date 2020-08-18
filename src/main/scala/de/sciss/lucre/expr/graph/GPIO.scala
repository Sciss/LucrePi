/*
 *  GPIO.scala
 *  (LucrePi)
 *
 *  Copyright (c) 2020 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package de.sciss.lucre.expr.graph

import com.pi4j.io.gpio.{GpioController, GpioFactory, GpioPinDigitalOutput, GpioProvider, PinState, Pin => JPin}
import de.sciss.lucre.expr.{Context, IControl, IExpr}
import de.sciss.lucre.stm.{Disposable, Sys}
import de.sciss.lucre.stm.TxnLike.peer

import scala.concurrent.stm.{Ref, TxnLocal}

object GPIO {
  private lazy val provider: GpioProvider   = GpioFactory.getDefaultProvider()

  private lazy val instance: GpioController = {
    try {
      GpioFactory.getInstance()
    } catch {
      case ex: UnsatisfiedLinkError =>  // e.g. trying to run on a desktop
        Console.err.println(s"GPIO - cannot obtain controller: ${ex.getMessage}")
        null
    }
  }

  private final class ExpandedDigitalOut[S <: Sys[S]](pin: IExpr[S, JPin], state: IExpr[S, Boolean] /*, tx0: S#Tx*/)
    extends IControl[S] {

    private[this] val local   = TxnLocal[Boolean](afterCommit = setState)
    private[this] val obsRef  = Ref(Disposable.empty[S#Tx])

    @volatile
    private[this] var out     = null: GpioPinDigitalOutput

//    local.set(state.value(tx0))(tx0.peer)

    private def setState(value: Boolean): Unit = {
      val _out = out
      if (_out != null) {
        _out.setState(value)
      }
    }

    def initControl()(implicit tx: S#Tx): Unit = {
      val jPin    = pin   .value
      val state0  = state .value
      tx.afterCommit {
        // XXX TODO --- is this good? does this always occur before TxnLocal update?
        val _instance = instance
        if (_instance != null) {
          out = _instance.provisionDigitalOutputPin(provider, jPin, if (state0) PinState.HIGH else PinState.LOW)
        }
      }
//      local() = state.value
      val obs = state.changed.react { implicit tx => upd =>
        local() = upd.now
      }
      obsRef() = obs
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      obsRef().dispose()
      tx.afterCommit {
        val _out = out
        if (_out != null) {
          instance.unprovisionPin(_out)
        }
      }
    }
  }

  object DigitalOut {
    /** Creates a digital output on the given pin reflecting the given low/high state. */
    def apply(pin: Pin, state: Ex[Boolean]): DigitalOut = Impl(pin, state)

    private final case class Impl(pin: Pin, state: Ex[Boolean]) extends DigitalOut {
      override def productPrefix: String = s"GPIO$$DigitalOut" // serialization

      type Repr[S <: Sys[S]] = IControl[S]

      protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
        new ExpandedDigitalOut[S](pin.expand[S], state.expand[S])
      }
    }
  }
  trait DigitalOut extends Control

  trait Pin extends Ex[JPin]
}
