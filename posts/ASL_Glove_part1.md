---
title: Designing a wireless gesture recognition glove, part 1
published: 2013-08-31
---


TODO:

get picture of prototype and us at the event

Last June, I participated in the MAKE with MOTO event where a few Motorola 
engineers and a van full of electronic parts stopped at Caltech. I joined 
two other Caltech students (Justin Koch and Rob Anderson) and two Pasadena 
Art Center design students (Walt Chiu and Joeseph Kan) to come with some 
crazy design and finish a prototype for it within 48 hours. We came up with 
an American Sign Language (ASL) glove. A glove that could recognize ASL 
gestures and translate to text/speech/whatever in close to real time on a 
smartphone. We think that such a device could allow for better communication 
between deaf and hearing people. 

<!--more-->

We managed to finish a proof of concept within that weekend which had 
conductive pads on key regions of the hand and fingers and a huge wire 
running from the glove to a Moduino (an Arduino DUE mounted to the back of a 
Motorola RAZR MAXX HD). We managed to finish the hardware but we ran out of time 
to actually complete the gesture recognition. The device also lacked 
accelerometer/gyro/compass (motion sensor) which prevented us from picking 
up actual gestures besides hand configurations. 


Today, I'm going to discuss my design process in building an actual prototype. 
The main weaknesses in the proof of concept was the bundle of wires going from 
the glove to the phone and the lack of a motion sensor which prevented true 
gesture recognition. I'm going to improve these areas by designing my own PCB 
with a motion sensor, bluetooth module and battery so that the glove can be 
completely wireless and smarter. Here are my main component choices and why I 
picked them:

1. Microcontroller: Atmel ATmega32U4
I knew I wanted a microcontroller supported by the Arduino environment for 
speed of implementation and the lovely reference schematics. I also wanted 
one that could communicate with my computer at the same time as communicating 
to the bluetooth module over serial. Therefore, the ATmega328 (used in the 
Arduino Uno) was out. I also wanted to minimize board area and power draw 
since this was going to sit on the back of a hand. So I chose the ATmega32U4 
in the Arduino Leonardo over the ATmega2560 or the ARM core that the Due uses. 
The ATmega32U4 has a built in USB controller for communicating with the 
computer but otherwise is a pretty standard atmel microcontroller. 


2. Bluetooth Module: RN-42
I chose the RN-42 because it seemed easy to integrate (just connect TX, RX, 
3V3, and GND). It has a built in antenna and is only 1" x 0.5" .

3. Motion sensor: Invensense MPU-9150
Combining 3 axis gryo, 3 axis accelerometer, and 3 axis compass into a 4mm by 4mm package,
the MPU-9150 seems like a great choice for a first try. If we find we need something with
more precision or more accuracy, then we can upgrade later. It has a nice and easy I2C 
interface and runs of 3V3.

4. Battery interface IC: TI BQ24075RGTT
We are running the board off a 1 cell Li-ion battery so we need a part to charge it and 
ideally automagically handle to choice between battery power and external power. The 
BQ24075RGTT does both of these things AND boost regulates the output voltage to 5.5V to
make the voltage conversion to 5V and 3V3 easy. This part also offers the ability to run 
without a battery attached which will be nice for debugging and programming as all I will 
need is a micro USB cable.

I'm starting out with parts that are easy to develop on and heavily-used by the hobbyist 
community to make it easy to get started. Part 2 will show the schematic and layout. Future
topics include assembly, first boot-up, and initial code development.