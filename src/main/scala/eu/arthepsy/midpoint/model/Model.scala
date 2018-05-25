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

abstract class Model[N, F] extends PartialModel[N, F] {
  val objectClass: String

  def objectFailure: F

  def toObject(op: OP): Either[F, ConnectorObject] = op match {
    case QUERY | UPDATE if this.isValidFor(op) =>
      this.toAttributes(op) match {
        case Right(attributes) =>
          val builder =
            new ConnectorObjectBuilder()
              .setObjectClass(new ObjectClass(objectClass))
          attributes.foreach(a => builder.addAttribute(a))
          Right(builder.build)
        case Left(err) => Left(err)
      }
    case _ =>
      Left(objectFailure)
  }
}

object Model {

  trait Object[M <: Model[N, F], N, F] extends PartialModel.Object[M, N, F] {

    def info: ObjectClassInfo

    def parse(uid: Uid, set: java.util.Set[Attribute]): Either[F, M] =
      internal.Model.parse(uid, set, parse, Left(parseFailure))

    def parse(uid: Uid, set: Set[Attribute]): Either[F, M] =
      internal.Model.parse(uid, set, parse)

    def toObject(native: N, op: OP): Either[F, ConnectorObject] = {
      val parsed = parse(native)
      parsed match {
        case Right(n) => n.toObject(op)
        case _        => parsed.asInstanceOf[Either[F, ConnectorObject]]
      }
    }

    def handle(handler: ResultsHandler, native: N): Boolean = {
      toObject(native, QUERY) match {
        case Right(o) => handler.handle(o)
        case _        => false
      }
    }
  }

}
