---
title: Raspberry Pi powered robotic bartender, part 1
date: 2014-01-22
---

The goal of this project is to build an automated bartender that can dispense 
different drinks automatically when given the set of drinks connected to it. 
Also, the machine should be accessible using a smartphone.

<!--more-->

Introduction
============

Justin Koch, project manager of the Caltech Robotics Team, handled the mechanical side namely how
transport the drink from their bottle into a cup. I am handling the 
electrical and programming side.

The design we settled on (Feb 2013), based on 
[this instructable](http://www.instructables.com/id/Fully-Automated-Bar-Barduino/)
is for an aquarium pump to pressurize the system and then use these
[valves](http://www.mcmaster.com/#catalog/120/491/=qd99x9) on each drink line
to dispense that drink. Now, I would suggest using these 
[peristaltic pumps](http://www.adafruit.com/products/1150) as they are more accurate
and easier to use. 

Electrical Design
=================

Since the valves are effectively a solenoid, controlling them is very straight
forward. All that is needed is an N-channel MOSFET on the low-side of the
solenoid and a flyback diode across the solenoid to prevent inductive 
voltage spikes when the solenoid is disconnected. Here's a schematic
of the control circuit for one solenoid.

<img src="/images/DrinkMachine-Control.png" alt="Control Circuit" style="width: 300px;"/>

The 10K pulldown resistor makes sure MOSFET is off during power up.

The next question was what would drive the MOSFET. Because we wanted people
to be able to order drinks from a web interface, we selected a Raspberry Pi
as it makes it easy to setup as a wireless access point, has enough
CPU resources to serve webpages easily and uses Linux. Because the 
Raspberry Pi has GPIO pins, all that is needed is to connect the
gates of the MOSFETs to the GPIO pins. 

Miscellaneous other parts needed include a 12V DC power supply
for powering the solenoids, blade connectors for connecting
to the solenoids, and my new favorite connector system,
Molex SL connectors for connecting between the main circuit,
Raspberry Pi and the solenoids.

Programming Design
==================

With the solenoids hooked up, all that's needed on the Raspberry Pi
side is code to control the GPIO pins. I chose Python as the implementation
language as there's a nice library, [RPi GPIO](https://pypi.python.org/pypi/RPi.GPIO)
for controlling the GPIO pins and since I want to design a web interface
Python has a lot of nice tools for web development. In terms of architecture,
I chose an OOP-based design as I think objects are a great way to model the state
of the machine and each drink recipe. The MachineState object holds the current
configuration of the machine such as what pins are connected to which drink
and how long to turn on a pin to output a given amount of liquid. The
Recipe objects holds instructions for making a drink and is stored
as a list of tuples of drink name and amount. Both the MachineState
and Recipe objects have class methods for contructing these objects
from configuration files allowing easy extension and configuration
of how the machine is setup and what possible drinks are there.

At the start of the process, the main Python programs reads
in the configuration file for the machine and setups the
MachineState object. Then, it reads the recipe file and filters
out recipes that can't be made based on the current configuration
of the machine. Finally, this main program uses the Flask 
web framework and handles the requests from web interface.
Using the list of possible drinks, it returns a order page
that contains only available drinks. 

Currently, the order process
is clunky as the machine pours the drink after receiving the POST
request from the form, and only returns when the drink is done.
One of the next features to implement is replacing the POST
request with AJAX and making the response more dynamic
by updating the page as each drink is poured.

First Implementation
====================

Here's what the first working demo looked like

![Demo](/images/DrinkMachineDemo.jpg)\

Next Steps
==========

After getting the demo working, there are a number of features and additions to
be made. First, I want to replace the huge, ugly perfboard with a small
simple PCB (that's the next post). Then, actually implement the machine
configuration file. After that comes building a drinks database and linking
that into Flask. Also, I want to replace the slow, synchronous POST request
and page change to order a drink with a AJAX-based UI that's a lot snappier.
Finally, I need to add admin functionality from the web interface so that
I can add different ingredients and "purge" (pump water through the lines
so that lines remain clean) the system without opening the system
or SSH'ing into the Raspberry Pi.
