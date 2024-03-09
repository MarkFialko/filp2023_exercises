package exercises04

import exercises04.Machine.Input.{Coin, Turn}

case class Machine(locked: Boolean, candies: Int, coins: Int)

/**
  * Реализуйте вендинговый аппарат по торговле барбарисками. Правила работы аппарата следующие:
  * если в закрытый аппарат вставляется монета (Coin), то аппартат открывается
  * если повернуть ручку (Turn) у открытого аппарата, то выйдет барбариска, и аппарат закроется
  * если в аппарате кончились барбариски, то он никак не реагирует. в этом случае надо вернуть список оставшихся Inputs и закончить
  * другие действия приводят к пропуску Input
  * если Input кончился, то заканчиваем
  * Подразумевается, что вы будете использовать паттерн-матчинг и рекурсию, так как while var isInstanceOf запрещены.
  */
object Machine {
  sealed trait Input
  object Input {
    case object Coin extends Input
    case object Turn extends Input
  }

  @scala.annotation.tailrec
  def run(machine: Machine, inputs: List[Input]): (Machine, List[Input]) = {
    inputs match {
      case Nil                             => (machine, Nil)
      case _ if machine.candies == 0       => (machine, inputs)
      case Coin :: tail if machine.locked  => run(machine.copy(locked = false, machine.candies, machine.coins + 1), tail)
      case Turn :: tail if !machine.locked => run(machine.copy(locked = true, machine.candies - 1, machine.coins), tail)
      case _ :: tail                       => run(machine, tail)
    }
  }
}
