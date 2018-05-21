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

package eu.arthepsy.midpoint.utils

import java.util
import org.identityconnectors.framework.common.exceptions.InvalidAttributeValueException
import org.identityconnectors.framework.common.objects.Attribute
import scala.collection.JavaConverters._

object Attributes {

  def contains(xs: util.Set[Attribute], name: String): Boolean =
    xs.asScala.exists(_.getName == name)

  def getValue[T](attr: Attribute, classz: Class[T]): Option[T] =
    this.getValue(attr.getValue, attr.getName, classz)

  def getValue[T](values: util.List[AnyRef],
                  name: String,
                  classz: Class[T]): Option[T] = {
    if (Option(values).isEmpty || values.isEmpty) {
      return None
    }
    if (values.size > 1) {
      throw new InvalidAttributeValueException(
        s"Multiple values for single valued attribute: $name")
    }
    val value = values.get(0)
    if (Option(value).isEmpty) {
      return None
    }
    if (classz.isAssignableFrom(value.getClass)) {
      return Option(value.asInstanceOf[T])
    }
    throw new InvalidAttributeValueException(
      s"Invalid value type ${value.getClass.getName} for attribute $name")
  }

  def getValue[T](xs: util.Set[Attribute],
                  name: String,
                  classz: Class[T]): Option[T] = {
    for (attr <- xs.asScala) {
      if (Option(attr).exists(_.getName == name)) {
        return this.getValue(attr.getValue, name, classz)
      }
    }
    None
  }

  def getValues[T](attr: Attribute, classz: Class[T]): Seq[T] =
    this.getValues(attr.getValue, attr.getName, classz)

  def getValues[T](values: util.List[AnyRef],
                   name: String,
                   classz: Class[T]): Seq[T] = {
    if (Option(values).isEmpty || values.isEmpty) {
      return Seq.empty
    }
    for (i <- 0 until values.size) yield {
      val value = values.get(i)
      if (Option(value).isEmpty) {
        return Seq.empty
      }
      if (!classz.isAssignableFrom(value.getClass)) {
        throw new InvalidAttributeValueException(
          s"Invalid value type ${value.getClass.getName} for attribute $name")
      }
      value.asInstanceOf[T]
    }
  }

  def getValues[T](xs: util.Set[Attribute],
                   name: String,
                   classz: Class[T]): Seq[T] = {
    for (attr <- xs.asScala) {
      if (Option(attr).exists(_.getName == name)) {
        return getValues(attr.getValue, name, classz)
      }
    }
    Seq.empty
  }
}
