package fi.paranoid
package snippet

import util.Random.nextLong

object LongTime {
  def render = {
    Thread.sleep(25000)

    <div>
      This thread delayed 2500 milliseconds
    </div>
  }
}
