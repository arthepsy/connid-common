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

package eu.arthepsy.midpoint.model

import org.identityconnectors.framework.common.objects._
import scala.collection.JavaConverters._

abstract class PartialModel[N, F] {
  def isValidFor(op: OP): Boolean
  def toAttributes(op: OP): Either[F, Set[Attribute]]
  def toNative(op: OP): Either[F, N]
}

object PartialModel {
  trait Object[M <: PartialModel[N, F], N, F] extends ObjectBase[M, N, F] {
    def attrNames: Seq[String]
    def attrInfos: Seq[AttributeInfo]
    def attrFieldName(name: String): String = name
  }

  trait ObjectBase[M <: PartialModel[N, F], N, F] {
    def parse(native: N): Either[F, M]
    def parse(set: Set[Attribute]): Either[F, M]
    def parseFailure: F

    def parse(set: java.util.Set[Attribute]): Either[F, M] =
      Option(set) match {
        case Some(xs) => parse(xs.asScala.toSet)
        case _        => Left(parseFailure)
      }
  }
}
