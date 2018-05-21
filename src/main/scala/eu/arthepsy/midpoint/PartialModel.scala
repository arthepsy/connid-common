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

import java.util

import org.identityconnectors.common.StringUtil
import org.identityconnectors.framework.common.objects.{
  Attribute,
  AttributeInfo
}

abstract class PartialModel[N] {
  import Model.OP

  def isValidFor(op: OP): Boolean
  def toAttributes(op: OP): Option[java.util.Set[Attribute]]
  def toConnectorAttributes(op: OP): Set[Attribute]
  def toNative(op: OP): Option[N]

  protected def isBlank(x: String): Boolean =
    StringUtil.isBlank(x)

  protected def isBlank(x: Option[String]): Boolean =
    x.isEmpty || isBlank(x.orNull)

}

object PartialModel {
  trait Object[N, T <: PartialModel[N]] extends ObjectBase[N, T] {
    def attrNames: Seq[String]
    def attrInfos: Seq[AttributeInfo]
    def attrFieldName(name: String): String = name
  }

  trait ObjectBase[N, T <: PartialModel[N]] {
    def parse(native: N): Option[T]
    def parse(set: util.Set[Attribute]): Option[T]
  }
}