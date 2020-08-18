/*
 *  RPi.scala
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

import com.pi4j.io.gpio.{RaspiPin, Pin => JPin}
import de.sciss.lucre.event.ITargets
import de.sciss.lucre.expr.graph.impl.MappedIExpr
import de.sciss.lucre.expr.{Context, IExpr}
import de.sciss.lucre.stm.Sys
import de.sciss.numbers.Implicits._

import scala.annotation.switch

object RPi {
  private final class ExpandedPin[S <: Sys[S]](index: IExpr[S, Int], tx0: S#Tx)
                                              (implicit targets: ITargets[S])
    extends MappedIExpr[S, Int, JPin](index, tx0) {

    protected def mapValue(inValue: Int)(implicit tx: S#Tx): JPin =
      (inValue.clip(0, 13): @switch) match {
        case  0 => RaspiPin.GPIO_00
        case  1 => RaspiPin.GPIO_01
        case  2 => RaspiPin.GPIO_02
        case  3 => RaspiPin.GPIO_03
        case  4 => RaspiPin.GPIO_04
        case  5 => RaspiPin.GPIO_05
        case  6 => RaspiPin.GPIO_06
        case  7 => RaspiPin.GPIO_07
        case  8 => RaspiPin.GPIO_08
        case  9 => RaspiPin.GPIO_09
        case 10 => RaspiPin.GPIO_10
        case 11 => RaspiPin.GPIO_11
        case 12 => RaspiPin.GPIO_12
        case 13 => RaspiPin.GPIO_13
        case 14 => RaspiPin.GPIO_14
        case 15 => RaspiPin.GPIO_15
        case 16 => RaspiPin.GPIO_16
        case 17 => RaspiPin.GPIO_17
        case 18 => RaspiPin.GPIO_18
        case 19 => RaspiPin.GPIO_19
        case 20 => RaspiPin.GPIO_20
        case 21 => RaspiPin.GPIO_21
        case 22 => RaspiPin.GPIO_22
        case 23 => RaspiPin.GPIO_23
        case 24 => RaspiPin.GPIO_24
        case 25 => RaspiPin.GPIO_25
        case 26 => RaspiPin.GPIO_26
        case 27 => RaspiPin.GPIO_27
        case 28 => RaspiPin.GPIO_28
        case 29 => RaspiPin.GPIO_29
        case 30 => RaspiPin.GPIO_30
        case 31 => RaspiPin.GPIO_31
      }
  }

  /** A pin on the Raspberry Pi GPIO.
   *
   * @param index the zero-based index on the GPIO. This is clipped to the valid
   *              range (0-31). This uses the <A HREF="http://wiringpi.com/pins/">Wiring-Pi scheme</A>,
   *              so index zero is physical pin 11 or pin number 17 in the BCM scheme. On the Pi 3B and 4B,
   *              there are effectively 21 GPIO pins, numbered 0 to 20.
   */
  final case class Pin(index: Ex[Int]) extends GPIO.Pin {
    override def productPrefix: String = s"RPi$$Pin" // serialization

    type Repr[S <: Sys[S]] = IExpr[S, JPin]

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      import ctx.targets
      new ExpandedPin[S](index.expand[S], tx)
    }
  }
}
