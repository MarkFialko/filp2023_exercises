package exercises02.game

import scala.util.control.Breaks.{break, breakable}

class Game(controller: GameController) {

  /**
    * Игра угадай число
    * Ввод и вывод необходимо осуществлять с помощью методов controller
    *
    * Игра должна вызывать controller.askNumber перед каждой попыткой игрока угадать число
    * И вызвать controller.nextLine для получения ввода игрока
    * Если игрок ввел число меньше загаданного, игра должна вызвать controller.numberIsBigger
    * Если игрок ввел число больше загаданного, игра должна вызвать controller.numberIsSmaller
    * Если игрок угадал число, игра должна закончиться и вызвать controller.guessed
    * Если игрок написал GameController.IGiveUp, игра должна закончиться и вызвать controller.giveUp(number)
    * Если игрок ввел неизвестную комбинацию символов, надо вызвать contoller.wrongInput и продолжить игру
    *
    * @param number загаданное число
    */
  def play(number: Int): Unit = {
    breakable {
      while (true) {
        controller.askNumber()
        val guess = controller.nextLine()

        if (guess == GameController.IGiveUp) {
          controller.giveUp(number)
          break
        }
        guess.toIntOption match {
          case Some(x) =>
            if (x != number) checkEqual(x, number)
            else {
              controller.guessed()
              break
            }
          case None => controller.wrongInput()
        }
      }
    }
  }
  private def checkEqual(x: Int, number: Int): Unit = {
    if (x > number) controller.numberIsSmaller()
    else controller.numberIsBigger()
  }
}
