/*
 * The MIT License
 * Copyright (c) 2018 Andris Raugulis (moo@arthepsy.eu)
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

package eu.arthepsy.midpoint

import org.identityconnectors.common.StringUtil
import org.identityconnectors.common.security.GuardedString

import scala.util.{Success, Try}

package object util {

  def isBlank(x: String): Boolean =
    StringUtil.isBlank(x)

  def isBlank(x: Option[String]): Boolean =
    x.isEmpty || isBlank(x.orNull)

  implicit class MutableSet[T](set: java.util.Set[T]) {
    def toMutable: java.util.Set[T] =
      Try {
        set.remove(None.orNull)
      } match {
        case Success(_) => set
        case _          => new java.util.HashSet(set)
      }
  }

  implicit class RevealingGuardedString(val gs: GuardedString) {
    def reveal: Option[String] = {
      val sb = StringBuilder.newBuilder
      Option(gs).foreach(_.access(new GuardedString.Accessor {
        override def access(chars: Array[Char]): Unit = {
          sb.append(new String(chars))
          ()
        }
      }))
      if (sb.nonEmpty) Some(sb.mkString) else None
    }
  }

}
