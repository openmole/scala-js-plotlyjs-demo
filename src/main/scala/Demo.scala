package plotlyjs.demo

import com.raquo.laminar.api.L._

/*
 * Copyright (C) 31/10/17 // mathieu.leclaire@openmole.org
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
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */


trait Demo {
  def elementDemo: ElementDemo
}

trait ElementDemo {
  def title: String

  def code: String

  def cleanCode = {
    if (code.startsWith("{")) code.tail.dropRight(1)
    else code
  }

  def element: HtmlElement

  def codeWidth: Int = 6
}