---
title: Designing a wireless gesture recognition glove, part 3
date: 2013-09-24
---

This is the third part of the wireless glove series. Part one is [here](/posts/ASL_Glove_part1.html) and part two is [here](/posts/ASL_Glove_part2.html). Today I'll talk about PCB layout. <!--more-->

Here is what the Rev1 board top layout looks like:

<a href="/images/Rev1_top_layout.png">

![Top layout](/images/Rev1_top_layout.png)\

</a>

Board Size and Design Constraints
=================================

I really wanted the complete board and battery to fit on the back of the hand for ergonomics and easy of connection to various sensors on the fingers and palm. In addition, I could keep all the components on the same board as the motion sensor needed to be on the hand anyway. Measuring my hand, I had an approximately 3.2" x 1.75" area to work with. I chose a rectangular board design to maximise the area I had to work with. However, Rev2 should use a rounded rectangle board as it still gives me lots of area to work with but lacks sharp corners that are undersirable on the back of someone's hand.

Passive and Misc. Component Selection
=====================================

I used 0805-sized parts wherever I could because then I have less footprints to manage and they are just big enough to hand solder easily. All the ceramic capacitors, resistors, and the one ferrite bead are 0805. The tantalum bulk capacitors are a slightly larger 1206. All the parts are surface mount so that the bottom of the board is flat and so the lithium battery can easily be placed underneath. 

Layout
======

I started by placing the major components on seperate areas of the board and organizing a fow from left-to-right. Power and USB come in from the left, go into the microcontroller in the middle and connect to the motion sensor and Bluetooth module on the right. I then routed the USB data lines through their terminating resistors and into the microcontroller so that I shouldn't have to worry about signal integrity or noise issues. Next, I added the components that need to be close to one of the major components such as bypass capacitors and the crystal. I then added the rest of the parts, which sounds trite, but I find layout to be a holisitic process so I often need to jiggle all the parts a few times to get everything to route and fit where I want it to. Finally, I add a ground plane on the front and two power planes (one 5V and one 3V3) both for aesthetics and cargo-culting ground planes even though this board doesn't require one.

ADD SOME INFO ABOUT SILKSCREENING

Final Renders
=============

Here are the OSH Park renders for the board:

<a href="/images/Rev1_top_render.png">

![Top render](/images/Rev1_top_render.png)\

</a>

<a href="/images/Rev1_bot_render.png">

![Top layout](/images/Rev1_bot_render.png)\

</a>