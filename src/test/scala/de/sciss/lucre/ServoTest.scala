//package de.sciss.lucre
//
//import com.pi4j.component.servo.impl.{GenericServo, PCA9685GpioServoProvider}
//import com.pi4j.gpio.extension.pca.{PCA9685GpioProvider, PCA9685Pin}
//import com.pi4j.io.gpio.GpioFactory
//import com.pi4j.io.i2c.{I2CBus, I2CFactory}
//
//object ServoTest {
//  def main(args: Array[String]): Unit = {
//    require (args.length >= 2, "Need two args: zero-based channel and angle in degrees")
//    val ch  = args(0).toInt
//    val ang = args(0).toInt
//    run(ch = ch, ang = ang)
//  }
//
//  def run(ch: Int, ang: Int): Unit = {
//    println("--1")
//    val gpioProvider  = createProvider()
//    val gpio          = GpioFactory.getInstance
//    /* val pin = */ gpio.provisionPwmOutputPin(gpioProvider, PCA9685Pin.PWM_00, "Servo 00")
//    val servoProvider = new PCA9685GpioServoProvider(gpioProvider)
//    val servo         = new GenericServo(servoProvider.getServoDriver(PCA9685Pin.PWM_00), "Servo_1")
//    servo.setPosition(0f)
//    println("--2")
//  }
//
//  def createProvider(): PCA9685GpioProvider = {
//    val bus = I2CFactory.getInstance(I2CBus.BUS_1)
//    new PCA9685GpioProvider(bus, 0x40)
//  }
//}
