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
import scala.collection.JavaConverters._

import model._
import util.Attributes._

class PartialModelSpec extends BaseFunSuite {

  implicit class RightBiasedEither[+A, +B](e: Either[A, B]) {
    def map[B1](f: B => B1): Either[A, B1] = e match {
      case Right(b) => Right(f(b))
      case _        => e.asInstanceOf[Either[A, B1]]
    }
  }

  class PartialDemoModel(val id: Int, val login: String)
      extends PartialModel[(Int, String), String] {

    override def isValidFor(op: OP): Boolean = {
      op match {
        case CREATE if id == 0 && !util.isBlank(login)                 => true
        case QUERY | UPDATE | DELETE if id > 0 && !util.isBlank(login) => true
        case _                                                         => false
      }
    }
    override def toAttributes(op: OP): Either[String, Set[Attribute]] = {
      if (isValidFor(op) && id % 3 == 0) {
        Right(
          Set(AttributeBuilder.build(Uid.NAME, id.toString),
              AttributeBuilder.build(Name.NAME, login)))
      } else {
        Left(s"not valid for $op")
      }
    }

    override def toNative(op: OP): Either[String, (Int, String)] = {
      if (isValidFor(op)) {
        Right((id, login))
      } else {
        Left(s"not valid for $op")
      }
    }
  }

  object PartialDemoModel
      extends PartialModel.Object[PartialDemoModel, (Int, String), String] {
    override def attrNames: Seq[String] = Seq("id")

    override def attrInfos: Seq[AttributeInfo] = Seq(
      new AttributeInfoBuilder(Uid.NAME, classOf[String])
        .setNativeName("demo.id")
        .setRequired(false)
        .setCreateable(false)
        .setUpdateable(false)
        .setReadable(true)
        .build,
      new AttributeInfoBuilder(Name.NAME, classOf[String])
        .setNativeName("login")
        .setRequired(true)
        .build()
    )

    override def parse(
        native: (Int, String)): Either[String, PartialDemoModel] = {
      native match {
        case (id, login) if id >= 0 && !util.isBlank(login) =>
          Right(new PartialDemoModel(id, login))
        case (_, login) if !util.isBlank(login) =>
          Left("id must be non-negative")
        case _ => Left("login must not be blank")
      }
    }

    override def parse(
        set: Set[Attribute]): Either[String, PartialDemoModel] = {
      (set.getValue(Uid.NAME, classOf[String]).map(_.toInt),
       set.getValue(Name.NAME, classOf[String])) match {
        case (Some(id), Some(login)) if id >= 0 && !util.isBlank(login) =>
          Right(new PartialDemoModel(id, login))
        case (_, Some(login)) if util.isBlank(login) =>
          Left("id must be non-negative")
        case _ => Left("login must not be blank")
      }
    }

    override def parseFailure: String = "parsing failure"
  }

  test("parse java attributes") {
    PartialDemoModel
      .parse(nullValue: java.util.Set[Attribute])
      .map(_.id) shouldBe a[Left[_, _]]
    PartialDemoModel
      .parse(new java.util.HashSet[Attribute](
        Seq(new Uid("123"), new Name("abc123")).asJavaCollection))
      .map(_.id) shouldBe Right(123)
  }

}
