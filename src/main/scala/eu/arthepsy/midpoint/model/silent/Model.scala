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

package eu.arthepsy.midpoint.model.silent

import org.identityconnectors.framework.common.objects._

import eu.arthepsy.midpoint.model.{OP, QUERY, UPDATE, internal}

abstract class Model[N] extends PartialModel[N] {
  val objectClass: String

  def toObject(op: OP): Option[ConnectorObject] = op match {
    case QUERY | UPDATE if this.isValidFor(op) =>
      this.toAttributes(op) match {
        case Some(attributes) =>
          val builder =
            new ConnectorObjectBuilder()
              .setObjectClass(new ObjectClass(objectClass))
          attributes.foreach(a => builder.addAttribute(a))
          Option(builder.build)
        case _ => None
      }
    case _ => None
  }
}

object Model {
  trait Object[M <: Model[N], N] extends PartialModel.Object[M, N] {

    def info: ObjectClassInfo

    def parse(uid: Uid, set: java.util.Set[Attribute]): Option[M] =
      internal.Model.parse(uid, set, parse, None)

    def parse(uid: Uid, set: Set[Attribute]): Option[M] =
      internal.Model.parse(uid, set, parse)

    def toObject(native: N, op: OP): Option[ConnectorObject] =
      parse(native).flatMap(_.toObject(op))

    def handle(handler: ResultsHandler, native: N): Boolean = {
      toObject(native, QUERY) match {
        case Some(o) => handler.handle(o)
        case _       => false
      }
    }
  }

}
