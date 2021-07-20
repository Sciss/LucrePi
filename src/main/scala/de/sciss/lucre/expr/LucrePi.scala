/*
 *  LucrePi.scala
 *  (LucrePi)
 *
 *  Copyright (c) 2020-2021 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package de.sciss.lucre.expr

object LucrePi {
  def init(): Unit = _init

  private lazy val _init: Unit = {
    ExElem.addProductReaderSq(graph.GPIO.DigitalIn :: graph.GPIO.DigitalOut :: graph.RPi.Pin :: Nil)
  }
}
