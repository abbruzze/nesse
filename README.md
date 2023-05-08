[![Build Status](https://app.travis-ci.com/abbruzze/nesse.svg?branch=main)](https://app.travis-ci.com/abbruzze/nesse)
[![Release](https://img.shields.io/github/v/release/abbruzze/nesse)](https://github.com/abbruzze/nesse/releases)
[![Language](https://img.shields.io/github/languages/top/abbruzze/nesse)]()
[![Downloads](https://img.shields.io/github/downloads/abbruzze/nesse/total)](https://github.com/abbruzze/nesse/releases/latest)

<p align="center">
  <img src="https://github.com/abbruzze/nesse/blob/main/images/logo.png">
</p>

Nesse a Scala 3 NES emulator
====

### What's new 1.1 (May 8th 2023)
- Added mapper 73 and 80. Fixed 4 and 69.
- Fixed USB joystick to avoid emulator freezing.
- Added general option to choose what to do with CPU jam.

Examples
====

<p align="center">
  <img src="https://github.com/abbruzze/nesse/blob/main/images/supermario.png">
  <img src="https://github.com/abbruzze/nesse/blob/main/images/castelvania_III.png">
  <img src="https://github.com/abbruzze/nesse/blob/main/images/arkanoid.png">
  <img src="https://github.com/abbruzze/nesse/blob/main/images/duckhuntvs.png">
  <img src="https://github.com/abbruzze/nesse/blob/main/images/zelda.png">
</p>

### Emulator features
-----------
- PAL/NTSC regions
- VS System
- Zapper
- R.O.B.
- FDS
- Cheats
- Name table & Tile viewer
- Snapshots: load/save and realtime snapshot saving; snapshots viewer
- Nescart DB
- Game Genie DB
- Controllers: keyboard, USB joystick
- Mappers
  - 0, 1, 2, 3, 4, 5, 7, 9, 10, 11, 13, 16, 18, 19, 20, 21, 22, 23, 25, 30, 33, 48, 65, 66, 68, 69, 71, 73, 79, 80, 87, 99, 107, 152, 185, 206, 210
- Debugger

### Download
-----------
Go to https://github.com/abbruzze/nesse/releases/latest and download and unzip on your computer the latest version.
Be sure to have a jre (14 or above) in the path and launch in the bin directory:
- On Windows: **nesse.bat**
- On Linux: **nesse.sh**

### Default keybindings
-----------

**Player 1**

 Button        | Mapped to
 --------------|-------------
 Start         | Enter
 Select        | Control
 A             | D
 B             | A
 Up            | Cursor Up
 Down          | Cursor Down
 Left          | Cursor Left
 Right         | Cursor Right


**Player 2**

 Button        | Mapped to
 --------------|-------------
 Start         | P
 Select        | O
 A             | T
 B             | U
 Up            | I
 Down          | M
 Left          | J
 Right         | L

### What's new in 1.0 (May 20th 2022)
First release
