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

import org.identityconnectors.framework.common.objects._

abstract class Model[N] extends PartialModel[N] {
  import Model.{QUERY, UPDATE}

  val objectClass: String

  def toObject(op: Model.OP): Option[ConnectorObject] = {
    op match {
      case QUERY | UPDATE if this.isValidFor(op) =>
      case _                                     => None
    }
    val builder =
      new ConnectorObjectBuilder()
        .setObjectClass(new ObjectClass(objectClass))
    this
      .toAttributes(op)
      .foreach(_.foreach(a => {
        builder.addAttribute(a)
        ()
      }))
    Option(builder.build)
  }
}

object Model {
  sealed trait OP
  case object QUERY extends OP
  case object CREATE extends OP
  case object UPDATE extends OP
  case object DELETE extends OP

  trait Object[N, T <: Model[N]] extends PartialModel.Object[N, T] {
    import scala.collection.JavaConverters._

    def info: ObjectClassInfo

    def parse(uid: Uid, set: java.util.Set[Attribute]): Option[T] =
      Option(set) match {
        case Some(xs) => parse(uid, xs.asScala.toSet)
        case _        => None
      }

    def parse(uid: Uid, set: Set[Attribute]): Option[T] = {
      if (Option(uid).isDefined) {
        if (Option(uid.getValue).isDefined) {
          parse(set + new Uid(uid.getUidValue))
        }
        if (Option(uid.getNameHint).isDefined && Option(uid.getNameHintValue).isDefined) {
          parse(set + new Name(uid.getNameHintValue))
        }
      }
      parse(set)
    }

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
