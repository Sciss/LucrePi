/*
 *  GPIO.scala
 *  (LucrePi)
 *
 *  Copyright (c) 2020-2022 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package de.sciss.lucre.expr.graph

import com.pi4j.io.gpio.event.{GpioPinDigitalStateChangeEvent, GpioPinListenerDigital}
import com.pi4j.io.gpio.{GpioController, GpioFactory, GpioPinDigitalInput, GpioPinDigitalOutput, GpioProvider, PinPullResistance, PinState, Pin => JPin}
import de.sciss.equal.Implicits._
import de.sciss.lucre.Txn.peer
import de.sciss.lucre.expr.ExElem.ProductReader
import de.sciss.lucre.expr.{Context, ExElem, IControl}
import de.sciss.lucre.impl.IChangeGeneratorEvent
import de.sciss.lucre.{Cursor, Disposable, IChangeEvent, IExpr, IPull, ITargets, Txn}
import de.sciss.model.Change
import de.sciss.proc.SoundProcesses

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

  private final class ExpandedDigitalOut[T <: Txn[T]](pin: IExpr[T, JPin], state: IExpr[T, Boolean] /*, tx0: T*/)
    extends IControl[T] {

    private[this] val local   = TxnLocal[Boolean](afterCommit = setState)
    private[this] val obsRef  = Ref(Disposable.empty[T])

    @volatile
    private[this] var out     = null: GpioPinDigitalOutput

//    local.set(state.value(tx0))(tx0.peer)

    private def setState(value: Boolean): Unit = {
      val _out = out
      if (_out != null) {
        _out.setState(value)
      }
    }

    def initControl()(implicit tx: T): Unit = {
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

    def dispose()(implicit tx: T): Unit = {
      obsRef().dispose()
      tx.afterCommit {
        val _out = out
        if (_out != null) {
          instance.unprovisionPin(_out)
        }
      }
    }
  }

  object DigitalOut extends ProductReader[DigitalOut] {
    /** Creates a digital output on the given pin reflecting the given low/high state. */
    def apply(pin: Pin, state: Ex[Boolean]): DigitalOut = Impl(pin, state)

    override def read(in: ExElem.RefMapIn, key: String, arity: Int, adj: Int): DigitalOut = {
      require (arity == 2 && adj == 0)
      val _pin      = in.readProductT[Pin]()
      val _state    = in.readEx[Boolean]()
      DigitalOut(_pin, _state)
    }

    private final case class Impl(pin: Pin, state: Ex[Boolean]) extends DigitalOut {
      override def productPrefix: String = s"GPIO$$DigitalOut" // serialization

      type Repr[T <: Txn[T]] = IControl[T]

      protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
        new ExpandedDigitalOut[T](pin.expand[T], state.expand[T])
      }
    }
  }
  /** A pin configured for digital output. The low/high state is specified in the constructor. */
  trait DigitalOut extends Control

  private final class ExpandedDigitalIn[T <: Txn[T]](pin: IExpr[T, JPin], state0: Boolean,
                                                     pull: IExpr[T, Option[Boolean]],
                                                     debounce: IExpr[T, Int] /*, tx0: T*/)
                                                    (implicit protected val targets: ITargets[T],
                                                     cursor: Cursor[T])
    extends IControl[T] with IExpr[T, Boolean] with IChangeGeneratorEvent[T, Boolean] {

    @volatile
    private[this] var in = null: GpioPinDigitalInput

    private[this] val valueRef = Ref(state0)

    private[this] lazy val listener = new GpioPinListenerDigital {
      def handleGpioPinDigitalStateChangeEvent(e: GpioPinDigitalStateChangeEvent): Unit = {
        val now = e.getState.isHigh
        setState(now)
      }
    }

    private def setState(now: Boolean): Unit =
      SoundProcesses.step[T]("GPIO.DigitalIn.handle") { implicit tx =>
        setStateTx(now)
      }

    private def setStateTx(now: Boolean)(implicit tx: T): Unit = {
      val before = valueRef.swap(now)
      if (now != before) {
        fire(Change(before, now))
      }
    }

    def value(implicit tx: T): Boolean = valueRef()

    override def changed: IChangeEvent[T, Boolean] = this

    private[lucre] def pullChange(pull: IPull[T])(implicit tx: T, phase: IPull.Phase): Boolean =
      pull.resolveExpr(this)

    def initControl()(implicit tx: T): Unit = {
      val jPin    = pin     .value
      val pull0   = pull    .value
      val deb0    = debounce.value
      // setStateTx(value0) // valueRef()  = value0 // a bit of a hack; assume an 'opener' circuit
      tx.afterCommit {
        val _instance = instance
        if (_instance != null) {
          val resistance = pull0 match {
            case Some(true)   => PinPullResistance.PULL_UP
            case Some(false)  => PinPullResistance.PULL_DOWN
            case None         => PinPullResistance.OFF
          }
          val _in = _instance.provisionDigitalInputPin(provider, jPin, resistance)
          if (deb0 >= 0) _in.setDebounce(deb0)
          _in.addListener(listener)
          val state1 = _in.getState.isHigh
          if (state1 !== state0) setState(state1)
          in = _in
        }
      }
    }

    def dispose()(implicit tx: T): Unit = {
      tx.afterCommit {
        val _in = in
        if (_in != null) {
          _in.removeListener(listener)
          instance.unprovisionPin(_in)
        }
      }
    }
  }

  val PullUp  : Ex[Option[Boolean]] = Some(true)
  val PullDown: Ex[Option[Boolean]] = Some(false)
  val PullOff : Ex[Option[Boolean]] = None

  object DigitalIn extends ProductReader[DigitalIn] {
    /** Creates a digital input on the given pin. It can be used to attach and listen to
      * buttons on the GPIO, for example.
      *
      * ''Note:'' Because initialization takes place outside a transaction, the value of the pin
      * is initially unknown and thus can be given as `init`. When initialized, this pin is actually polled,
      * potentially triggering actions in the user code.
      *
      * @param  pin       the pin to poll
      * @param  pull      if defined, sets a pull-up (`true`) or pull-down (`false`) resistor. Defaults to `None`.
      * @param  init      the assumed initial state (defaults to `false`)
      * @param  debounce  if zero or positive, specifies a debounce option in milliseconds.
      *                   Debouncing is used when due to noise or imprecision multiple button
      *                   clicks are detected when they should be treated as one. Defaults to `-1`.
      */
    def apply(pin: Pin, pull: Ex[Option[Boolean]] = None, init: Ex[Boolean] = false, debounce: Ex[Int] = -1): DigitalIn =
      Impl(pin, pull, init, debounce)

    override def read(in: ExElem.RefMapIn, key: String, arity: Int, adj: Int): DigitalIn = {
      require (arity == 4 && adj == 0)
      val _pin      = in.readProductT[Pin]()
      val _pull     = in.readEx[Option[Boolean]]()
      val _init     = in.readEx[Boolean]()
      val _debounce = in.readEx[Int]()
      DigitalIn(_pin, _pull, _init, _debounce)
    }

    private final case class Impl(pin: Pin, pull: Ex[Option[Boolean]], init: Ex[Boolean], debounce: Ex[Int])
      extends DigitalIn {

      override def productPrefix: String = s"GPIO$$DigitalIn" // serialization

      type Repr[T <: Txn[T]] = IControl[T] with IExpr[T, Boolean]

      protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
        import ctx.{cursor, targets}
        new ExpandedDigitalIn[T](pin.expand[T], pull = pull.expand[T], debounce = debounce.expand[T],
          state0 = init.expand[T].value)
      }
    }
  }
  /** A pin configured for digital input. The expression reflects the pin's state
    * as low (`false`) or high (`true`).
    */
  trait DigitalIn extends Control with Ex[Boolean] {
    type Repr[T <: Txn[T]] <: IControl[T] with IExpr[T, Boolean]
  }

  trait Pin extends Ex[JPin]
}
