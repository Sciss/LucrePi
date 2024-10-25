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
import com.pi4j.io.i2c.{I2CBus, I2CDevice, I2CFactory}
import de.sciss.equal.Implicits._
import de.sciss.lucre.Txn.peer
import de.sciss.lucre.expr.ExElem.{ProductReader, RefMapIn}
import de.sciss.lucre.expr.impl.IActionImpl
import de.sciss.lucre.expr.{Context, ExElem, IAction, IControl, ITrigger}
import de.sciss.lucre.impl.{IChangeEventImpl, IChangeGeneratorEvent, IEventImpl, IGeneratorEvent}
import de.sciss.lucre.{Cursor, Disposable, IChangeEvent, IEvent, IExpr, IPull, ITargets, Txn}
import de.sciss.model.Change
import de.sciss.proc.SoundProcesses

import scala.concurrent.stm.{Ref, TArray, TxnLocal}

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

  // ---------- ADS1X15 ----------

  object ADS1X15 extends ProductReader[ADS1X15] {
    /**
      * @param bus      i2c bus (default is 1)
      * @param address  i2c address (default is 0x48)
      * @param bits     16 for the ADS1115, 12 for the ADS1015 (default is 16)
      */
    def apply(bus: Ex[Int] = 1, address: Ex[Int] = 0x48, bits: Ex[Int] = 16): ADS1X15 =
      Impl(bus = bus, address = address, bits = bits)

    override def read(in: ExElem.RefMapIn, key: String, arity: Int, adj: Int): ADS1X15 = {
      require (arity == 3 && adj == 0)
      val _bus      = in.readEx[Int]()
      val _address  = in.readEx[Int]()
      val _bits     = in.readEx[Int]()
      ADS1X15(_bus, _address, _bits)
    }

//    object Received extends ProductReader[Received] {
//      override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Received = {
//        require (arity == 1 && adj == 0)
//        val _a = in.readProductT[ADS1X15]()
//        new Received(_a)
//      }
//    }
//    final case class Received(a: ADS1X15) extends Trig {
//      type Repr[T <: Txn[T]] = ITrigger[T]
//
//      override def productPrefix = s"GPIO$$ADS1X15$$Received"   // serialization
//
//      protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
//        val ns = a.expand[T]
//        import ctx.targets
//        new ReceivedExpanded[T](ns, tx)
//      }
//    }

    object RunSingle extends ProductReader[RunSingle] {
      override def read(in: RefMapIn, key: String, arity: Int, adj: Int): RunSingle = {
        assert (arity == 2 && adj == 0)
        val _a    = in.readProductT[ADS1X15]()
        val _chan = in.readEx[Int]()
        new RunSingle(_a, _chan)
      }
    }
    final case class RunSingle(a: ADS1X15, chan: Ex[Int]) extends Act with Trig {
      type Repr[T <: Txn[T]] = IAction[T] with ITrigger[T]

      override def productPrefix: String = s"GPIO$$ADS1X15$$RunSingle" // serialization

      protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
        val ax      = a.expand[T]
        val chanEx  = chan.expand[T]
        import ctx.targets
        new ExpandedRunSingle[T](ax, chanEx, tx)
      }
    }

    private final class ExpandedRunSingle[T <: Txn[T]](a: ADS1X15.Repr[T], chan: IExpr[T, Int], tx0: T)
                                                      (implicit protected val targets: ITargets[T])
      extends IActionImpl[T] with ITrigger[T] with IEventImpl[T, Unit] {

      private final val chanRef = Ref(-1)

      a.received.--->(changed)(tx0)

      override def executeAction()(implicit tx: T): Unit = {
        val chanV = chan.value
        if (chanV >= 0 && chanV < 4) {
          chanRef() = chanV
          a.runSingle(chanV)
        }
      }

      override def dispose()(implicit tx: T): Unit = {
        a.received.-/->(changed)
        super.dispose()
      }

      override def changed: IEvent[T, Unit] = this

      override private[lucre] def pullUpdate(pull: IPull[T])(implicit tx: T): Option[Unit] = {
        val opt = pull(a.received)
        if (opt.isDefined) Trig.Some else None  // XXX TODO is this correct?
      }
    }

    object In extends ProductReader[In] {
      override def read(in: RefMapIn, key: String, arity: Int, adj: Int): In = {
        require (arity == 2 && adj == 0)
        val _a    = in.readProductT[ADS1X15]()
        val _chan = in.readEx[Int]()
        new In(_a, _chan)
      }
    }
    final case class In(a: ADS1X15, chan: Ex[Int]) extends Ex[Int] {
      type Repr[T <: Txn[T]] = IExpr[T, Int]

      override def productPrefix = s"OscUdpNode$$Sender"   // serialization

      protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
        val ax      = a.expand[T]
        val chanEx  = chan.expand[T]
        import ctx.targets
        new InExpanded(ax, chanEx, tx)
      }
    }

    private final class InExpanded[T <: Txn[T]](a: Repr[T], chan: IExpr[T, Int], tx0: T)
                                               (implicit protected val targets: ITargets[T])
      extends IExpr[T, Int] with IChangeEventImpl[T, Int] {

      a.received.--->(changed)(tx0)

      def value(implicit tx: T): Int = a.in(chan.value)

      private[lucre] def pullChange(pull: IPull[T])(implicit tx: T, phase: IPull.Phase): Int = {
        /*val opt =*/ pull(a.received)
        a.in(chan.value) // XXX TODO is this correct?
      }

      def dispose()(implicit tx: T): Unit =
        a.received.-/->(changed)

      def changed: IChangeEvent[T, Int] = this
    }

    private final case class Impl(bus: Ex[Int], address: Ex[Int], bits: Ex[Int]) extends ADS1X15 {
      override def productPrefix: String = s"GPIO$$ADS1X15" // serialization

//      override def received         : Trig    = Received(this)

      override def runSingle(chan: Ex[Int]): Act with Trig = RunSingle(this, chan)

      override def in(chan: Ex[Int]): Ex[Int] = In(this, chan)

      protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
        import ctx.{cursor, targets}
        new ExpandedADS1X15[T](bus = bus.expand[T], address = address.expand[T], bits = bits.expand[T])
      }
    }

    trait Repr[T <: Txn[T]] extends IControl[T] {
      def in(chan: Int)(implicit tx: T): Int

      def runSingle(chan: Int)(implicit tx: T): Unit

      def received: IEvent[T, (Int, Int)]
    }
  }
  trait ADS1X15 extends Control {
    type Repr[T <: Txn[T]] = ADS1X15.Repr[T]

//    /** Triggers when a conversion is completed. */
//    def received(chan: Ex[Int]): Trig

    /** Runs a single conversion on a given channel. The trigger is fired when conversion is complete. */
    def runSingle(chan: Ex[Int]): Act with Trig

    /** The last conversion at a given analog input. */
    def in(chan: Ex[Int]): Ex[Int]
  }

  // -----
  // implementation based on `Adafruit_ADS1X15`, licensed under BSD License:
  // Copyright (c) 2012, Adafruit Industries, all rights reserved.
  // -----

  private final val ADS1X15_REG_POINTER_CONFIG      = 0x01  // Configuration
  private final val ADS1X15_REG_POINTER_LOWTHRESH   = 0x02  // Low threshold
  private final val ADS1X15_REG_POINTER_HITHRESH    = 0x03  // High threshold
  private final val ADS1X15_REG_POINTER_CONVERT     = 0x00  // Conversion

//  private final val ADS1X15_REG_CONFIG_CQUE_1CONV   = 0x0000 // Assert ALERT/RDY after one conversions
//  private final val ADS1X15_REG_CONFIG_CLAT_NONLAT  = 0x0000 // Non-latching comparator (default)
//  private final val ADS1X15_REG_CONFIG_CPOL_ACTVLOW = 0x0000 // ALERT/RDY pin is low when active (default)
//  private final val ADS1X15_REG_CONFIG_CMODE_TRAD   = 0x0000 // Traditional comparator with hysteresis (default)
  private final val ADS1X15_REG_CONFIG_MODE_CONTIN  = 0x0000 // Continuous conversion mode
  private final val ADS1X15_REG_CONFIG_MODE_SINGLE  = 0x0100 // Power-down single-shot mode (default)
  private final val ADS1X15_REG_CONFIG_OS_SINGLE    = 0x8000 // Write: Set to start a single-conversion

//  private final val ADS1X15_REG_CONFIG_MUX_SINGLE_0 = 0x4000 // Single-ended AIN0
//  private final val ADS1X15_REG_CONFIG_MUX_SINGLE_1 = 0x5000 // Single-ended AIN1
//  private final val ADS1X15_REG_CONFIG_MUX_SINGLE_2 = 0x6000 // Single-ended AIN2
//  private final val ADS1X15_REG_CONFIG_MUX_SINGLE_3 = 0x7000 // Single-ended AIN3

  private final val ADS1X15_REG_CONFIG_PGA_6_144V   = 0x0000 // +/-6.144V range = Gain 2/3

  private final val RATE_ADS1015_1600SPS            = 0x0080 // 1600 samples per second (default)
  private final val RATE_ADS1115_128SPS             = 0x0080 //  128 samples per second (default)

  private final class ExpandedADS1X15[T <: Txn[T]](bus: IExpr[T, Int], address: IExpr[T, Int], bits: IExpr[T, Int])
                                                  (implicit protected val targets: ITargets[T], cursor: Cursor[T])
    extends ADS1X15.Repr[T] with IGeneratorEvent[T, (Int, Int)] {

//    private[this] val obsRef  = Ref(Disposable.empty[T])

    private[this] val gain      = ADS1X15_REG_CONFIG_PGA_6_144V    // GAIN_TWOTHIRDS - +/- 6.144V range (limited to VDD +0.3V max!)
    private[this] var bitShift  = 0
    private[this] var dataRate  = 0

    @volatile
    private[this] var i2cBus    = null: I2CBus
    @volatile
    private[this] var i2cDev    = null: I2CDevice

    private[this] val _values   = TArray.ofDim[Int](4) // new Array[Int](4)

    override def in(chan: Int)(implicit tx: T): Int =
      if (chan >= 0 && chan < 4) _values(chan) else 0

    override def runSingle(chan: Int)(implicit tx: T): Unit =
      tx.afterCommit {
        val mux = (chan + 4) << 12  // 0x4000, 0x5000 etc.
        startADCReading(mux, continuous = false)
        // Wait for the conversion to complete
        while (!conversionComplete()) ()
        val value = getLastConversionResults()
//        _values(chan) = value
        SoundProcesses.step[T]("ADS1X15.runSingle") { implicit tx =>
          _values(chan) = value
          fire((chan, value))
        }
      }

    override private[lucre] def pullUpdate(pull: IPull[T])(implicit tx: T): Option[(Int, Int)] =
      Some(pull.resolve[(Int, Int)])

    override def received: IEvent[T, (Int, Int)] = this

    private def getLastConversionResults(): Int = {
      // Read the conversion results
      val res = readRegister(ADS1X15_REG_POINTER_CONVERT) >> bitShift
      if (bitShift == 0 || res <= 0x07FF) {
        res
      } else {
        // Shift 12-bit results right 4 bits for the ADS1015,
        // making sure we keep the sign bit intact
        // negative number - extend the sign to 16th bit
        res | 0xF000
      }
    }

    private def startADCReading(mux: Int, continuous: Boolean): Unit = {
      // Start with default values
      val config =
//        ADS1X15_REG_CONFIG_CQUE_1CONV |   // Set CQUE to any value other than
//          // None so we can use it in RDY mode
//          ADS1X15_REG_CONFIG_CLAT_NONLAT |  // Non-latching (default val)
//          ADS1X15_REG_CONFIG_CPOL_ACTVLOW | // Alert/Rdy active low   (default val)
//          ADS1X15_REG_CONFIG_CMODE_TRAD     // Traditional comparator (default val)
        (if (continuous) ADS1X15_REG_CONFIG_MODE_CONTIN else ADS1X15_REG_CONFIG_MODE_SINGLE) | gain | dataRate | mux | ADS1X15_REG_CONFIG_OS_SINGLE

      // Write config register to the ADC
      writeRegister(ADS1X15_REG_POINTER_CONFIG    , config)
      // Set ALERT/RDY to RDY mode.
      writeRegister(ADS1X15_REG_POINTER_HITHRESH  , 0x8000)
      writeRegister(ADS1X15_REG_POINTER_LOWTHRESH , 0x0000)
    }

    private def conversionComplete(): Boolean =
      (readRegister(ADS1X15_REG_POINTER_CONFIG) & 0x8000) != 0

    private def readRegister(reg: Int): Int = {
      val arr = new Array[Byte](2)
      i2cDev.write(reg.toByte)
      i2cDev.read (arr, 0, 2)
      ((arr(0) & 0xFF) << 8) | (arr(1) & 0xFF)
    }

    private def writeRegister(reg: Int, value: Int): Unit = {
      val arr = new Array[Byte](3)
      arr(0) = reg.toByte
      arr(1) = (value >> 8).toByte
      arr(2) = (value & 0xFF).toByte
      i2cDev.write(arr)
    }

    def initControl()(implicit tx: T): Unit = {
      val busV      = bus     .value
      val addressV  = address .value
      val bitsV     = bits    .value

      bitShift    = 16 - bitsV
      if (bitsV == 12) {
        dataRate  = RATE_ADS1015_1600SPS
      } else {
        if (bitsV != 16) Console.err.println(s"Warning: ADS1X15 number of bits ($bitsV) must be 12 or 16")
        dataRate  = RATE_ADS1115_128SPS
      }

      tx.afterCommit {
        i2cBus = I2CFactory.getInstance(busV)
        i2cDev = i2cBus.getDevice(addressV)
      }
    }

    def dispose()(implicit tx: T): Unit = {
//      obsRef().dispose()
      tx.afterCommit {
        val _i2cBus = i2cBus
        if (_i2cBus != null) {
          _i2cBus.close()
        }
      }
    }
  }
}
