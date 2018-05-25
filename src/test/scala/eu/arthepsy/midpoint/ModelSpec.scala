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

class ModelSpec extends BaseFunSuite {

  implicit class RightBiasedEither[+A, +B](e: Either[A, B]) {
    def map[B1](f: B => B1): Either[A, B1] = e match {
      case Right(b) => Right(f(b))
      case _        => e.asInstanceOf[Either[A, B1]]
    }
  }

  class DemoModel(val id: Int, val login: String)
      extends Model[(Int, String), String] {
    override val objectClass: String = "demo"

    override def objectFailure: String = "object failure"

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

  object DemoModel extends Model.Object[DemoModel, (Int, String), String] {
    override def info: ObjectClassInfo =
      new ObjectClassInfoBuilder()
        .setType("demo")
        .addAllAttributeInfo(attrInfos.asJava)
        .build()

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

    override def parse(native: (Int, String)): Either[String, DemoModel] = {
      native match {
        case (id, login) if id >= 0 && !util.isBlank(login) =>
          Right(new DemoModel(id, login))
        case (_, login) if !util.isBlank(login) =>
          Left("id must be non-negative")
        case _ => Left("login must not be blank")
      }
    }

    override def parse(set: Set[Attribute]): Either[String, DemoModel] = {
      (set.getValue(Uid.NAME, classOf[String]).map(_.toInt),
       set.getValue(Name.NAME, classOf[String])) match {
        case (Some(id), Some(login)) if id >= 0 && !util.isBlank(login) =>
          Right(new DemoModel(id, login))
        case (_, Some(login)) if util.isBlank(login) =>
          Left("id must be non-negative")
        case _ => Left("login must not be blank")
      }
    }

    override def parseFailure: String = "parsing failure"
  }

  test("parse java attributes") {
    DemoModel
      .parse(nullValue, nullValue: java.util.Set[Attribute])
      .map(_.id) shouldBe a[Left[_, _]]
    DemoModel
      .parse(new Uid("123"),
             new java.util.HashSet[Attribute](
               Seq(new Name("abc123")).asJavaCollection))
      .map(_.id) shouldBe Right(123)
  }

  test("parse attributes with uid") {
    DemoModel
      .parse(new Uid("123"), Set[Attribute](new Name("abc123")))
      .map(_.id) shouldBe Right(123)
    DemoModel
      .parse(nullValue, Set[Attribute](new Uid("123"), new Name("abc123")))
      .map(_.id) shouldBe Right(123)
    DemoModel
      .parse(new Uid("123"), Set[Attribute](new Uid("456"), new Name("abc123")))
      .map(_.id) shouldBe Right(123)
  }

  test("parse attributes with name") {
    DemoModel
      .parse(new Uid("123", new Name("abc123")), Set.empty[Attribute])
      .map(_.login) shouldBe Right("abc123")
    DemoModel
      .parse(nullValue, Set[Attribute](new Uid("123"), new Name("abc123")))
      .map(_.login) shouldBe Right("abc123")
    DemoModel
      .parse(new Uid("123", new Name("abc123")),
             Set[Attribute](new Uid("456", new Name("def456"))))
      .map(_.login) shouldBe Right("abc123")
  }

  test("toObject") {
    DemoModel.toObject((123, "abc123"), QUERY) shouldBe a[Right[_, _]]
    DemoModel.toObject((-123, "abc123"), QUERY) shouldBe a[Left[_, _]]
    DemoModel.toObject((123, ""), QUERY) shouldBe a[Left[_, _]]
    DemoModel.toObject((123, "abc123"), UPDATE) shouldBe a[Right[_, _]]
    DemoModel.toObject((-123, "abc123"), UPDATE) shouldBe a[Left[_, _]]
    DemoModel.toObject((123, ""), UPDATE) shouldBe a[Left[_, _]]
    DemoModel.toObject((123, "abc123"), DELETE) shouldBe a[Left[_, _]]
    DemoModel.toObject((123, "abc123"), CREATE) shouldBe a[Left[_, _]]
    DemoModel.toObject((125, "abc125"), QUERY) shouldBe a[Left[_, _]]
    DemoModel.toObject((125, "abc125"), UPDATE) shouldBe a[Left[_, _]]
  }

  test("handle") {
    val handler = new ResultsHandler {
      override def handle(connectorObject: ConnectorObject): Boolean = true
    }
    DemoModel.handle(handler, (123, "abc123"))
    DemoModel.handle(handler, (-123, "abc123"))
    DemoModel.handle(handler, (123, ""))
  }

}
