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

abstract class Model[N, F] extends PartialModel[N, F] {
  val objectClass: String

  def objectFailure: F

  def toObject(op: OP): Either[F, ConnectorObject] = {
    op match {
      case QUERY | UPDATE if this.isValidFor(op) =>
      case _                                     => Left(objectFailure)
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
    Right(builder.build)
  }
}

object Model {

  trait Object[M <: Model[N, F], N, F] extends PartialModel.Object[M, N, F] {

    def info: ObjectClassInfo

    def parse(uid: Uid, set: java.util.Set[Attribute]): Either[F, M] =
      Option(set) match {
        case Some(xs) => parse(uid, xs.asScala.toSet)
        case _        => Left(parseFailure)
      }

    def parse(uid: Uid, set: Set[Attribute]): Either[F, M] = {
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

    def toObject(native: N, op: OP): Either[F, ConnectorObject] =
      parse(native).flatMap(_.toObject(op))

    def handle(handler: ResultsHandler, native: N): Boolean = {
      toObject(native, QUERY) match {
        case Right(o) => handler.handle(o)
        case _        => false
      }
    }
  }

}
