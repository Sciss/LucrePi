# LucrePi

[![Build Status](https://github.com/Sciss/LucrePi/workflows/Scala%20CI/badge.svg?branch=main)](https://github.com/Sciss/LucrePi/actions?query=workflow%3A%22Scala+CI%22)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/de.sciss/lucrepi_2.13/badge.svg)](https://maven-badges.herokuapp.com/maven-central/de.sciss/lucrepi_2.13)

## statement

LucrePi is a Scala library which bridges between the Raspberry Pi GPIO (through 
the [Pi4j](https://github.com/Pi4J/pi4j/) project) and [Lucre](https://github.com/Sciss/Lucre/).
It is (C)opyright 2020–2021 by Hanns Holger Rutz. All rights reserved. The project is released under
the [GNU Affero General Public License](https://github.com/Sciss/LucrePi/raw/main/LICENSE) v2.1+ and comes 
with absolutely no warranties. To contact the author, send an e-mail to `contact at sciss.de`.

## requirements / building

This project builds with sbt against Scala 2.12, 2.13, Dotty.

To use the library in your project:

    "de.sciss" %% "lucrepi" % v

The current version `v` is `"1.5.1"`.

## contributing

Please see the file [CONTRIBUTING.md](CONTRIBUTING.md)

