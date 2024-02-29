package exercises03.game

object Game {
  def parseState(input: String, number: Int): State = {
    input.toIntOption match {
      case Some(userNumber) if userNumber == number => Guessed
      case Some(userNumber) if userNumber < number  => NumberIsBigger
      case Some(userNumber) if userNumber > number  => NumberIsSmaller
      case None if input == GameController.IGiveUp  => GiveUp
      case _                                        => WrongInput
    }
  }

  def action(state: State, number: Int): GameController => Unit = {
    state match {
      case GiveUp          => gm: GameController => gm.giveUp(number)
      case WrongInput      => gm: GameController => gm.wrongInput()
      case NumberIsBigger  => gm: GameController => gm.numberIsBigger()
      case NumberIsSmaller => gm: GameController => gm.numberIsSmaller()
      case Guessed         => gm: GameController => gm.guessed()
    }
  }

  def completed(state: State): Boolean = {
    if (state == GiveUp || state == Guessed) true
    else false
  }
}
