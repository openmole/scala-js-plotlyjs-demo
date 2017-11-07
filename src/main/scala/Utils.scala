package plotlyjs.demo
/*
 * Copyright (C) 24/03/16 // mathieu.leclaire@openmole.org
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 */

import scala.scalajs.js.JSConverters._
object Utils {

  lazy val rng = scala.util.Random

  def randomDoubles(nb: Int = 100, ratio: Int = 1000) = Seq.fill(nb)(rng.nextDouble * ratio).toJSArray

  def randomInts(nb: Int = 100, ratio: Int = 1000) = Seq.fill(nb)(rng.nextInt(ratio)).toJSArray
}
