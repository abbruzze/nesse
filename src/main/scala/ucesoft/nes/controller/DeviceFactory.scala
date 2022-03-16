package ucesoft.nes.controller

import ucesoft.nes.controller.rob.{ROB, ROBGyromiteController, ROBStackUpController}
import ucesoft.nes.cpu.Memory
import ucesoft.nes.util.CartDB.Game

object DeviceFactory:
  var keyboardLayout : FamilyKeyboard.KeyboardLayout = FamilyKeyboard.DEFAULT_LAYOUT
  def deviceFrom(it:InputType,game:Game,mem:Memory,controllerA:Joystick,controllerB:Joystick) : InputDevice =
    it match
      case InputType.FamilyKeyboard => new FamilyKeyboard(keyboardLayout)
      case InputType.Zapper => new Zapper(2)
      case InputType.VSZapper => new Zapper(1,true)
      case InputType.ROB =>
        game.crc match
          case 0x97B0F110 | 0xDF67DAA1 => new ROB(new ROBStackUpController(mem,controllerA,controllerB)) // Stack-up
          case 0x023A5A32 | 0x84EF2FF9 => new ROB(new ROBGyromiteController(mem,controllerA,controllerB)) // Gyromite

  def deviceFrom(crc:Long,mem:Memory,controllerA:Joystick,controllerB:Joystick): Option[InputDevice] =
    crc match
      case 0x97B0F110 | 0xDF67DAA1 => Some(new ROB(new ROBStackUpController(mem,controllerA,controllerB))) // Stack-up
      case 0x023A5A32 | 0x84EF2FF9 => Some(new ROB(new ROBGyromiteController(mem,controllerA,controllerB))) // Gyromite
      case _ => None
