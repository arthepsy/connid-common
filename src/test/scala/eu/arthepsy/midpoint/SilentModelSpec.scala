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

class SilentModelSpec extends BaseFunSuite {

  class DemoSilentModel(val id: Int, val login: String)
      extends silent.Model[(Int, String)] {
    override val objectClass: String = "demo"

    override def isValidFor(op: OP): Boolean = {
      op match {
        case CREATE if id == 0 && !util.isBlank(login)                 => true
        case QUERY | UPDATE | DELETE if id > 0 && !util.isBlank(login) => true
        case _                                                         => false
      }
    }
    override def toAttributes(op: OP): Option[Set[Attribute]] = {
      if (isValidFor(op) && id % 3 == 0) {
        Option(
          Set(AttributeBuilder.build(Uid.NAME, id.toString),
              AttributeBuilder.build(Name.NAME, login)))
      } else {
        None
      }
    }

    override def toNative(op: OP): Option[(Int, String)] = {
      if (isValidFor(op)) {
        Option((id, login))
      } else {
        None
      }
    }
  }

  object DemoSilentModel
      extends silent.Model.Object[(Int, String), DemoSilentModel] {
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

    override def parse(native: (Int, String)): Option[DemoSilentModel] = {
      native match {
        case (id, login) if id >= 0 && !util.isBlank(login) =>
          Option(new DemoSilentModel(id, login))
        case (_, login) if !util.isBlank(login) =>
          None
        case _ => None
      }
    }

    override def parse(set: Set[Attribute]): Option[DemoSilentModel] = {
      (set.getValue(Uid.NAME, classOf[String]).map(_.toInt),
       set.getValue(Name.NAME, classOf[String])) match {
        case (Some(id), Some(login)) if id >= 0 && !util.isBlank(login) =>
          Option(new DemoSilentModel(id, login))
        case (_, Some(login)) if util.isBlank(login) =>
          None
        case _ => None
      }
    }

  }

  test("parse java attributes") {
    DemoSilentModel
      .parse(nullValue: Uid, nullValue: java.util.Set[Attribute])
      .map(_.id) shouldBe None
    DemoSilentModel
      .parse(new Uid("123"),
             new java.util.HashSet[Attribute](
               Seq(new Name("abc123")).asJavaCollection))
      .map(_.id) shouldBe Some(123)
  }

  test("parse attributes with uid") {
    DemoSilentModel
      .parse(new Uid("123"), Set[Attribute](new Name("abc123")))
      .map(_.id) shouldBe Some(123)
    DemoSilentModel
      .parse(nullValue, Set[Attribute](new Uid("123"), new Name("abc123")))
      .map(_.id) shouldBe Some(123)
    DemoSilentModel
      .parse(new Uid("123"), Set[Attribute](new Uid("456"), new Name("abc123")))
      .map(_.id) shouldBe Some(123)
  }

  test("parse attributes with name") {
    DemoSilentModel
      .parse(new Uid("123", new Name("abc123")), Set.empty[Attribute])
      .map(_.login) shouldBe Some("abc123")
    DemoSilentModel
      .parse(nullValue, Set[Attribute](new Uid("123"), new Name("abc123")))
      .map(_.login) shouldBe Some("abc123")
    DemoSilentModel
      .parse(new Uid("123", new Name("abc123")),
             Set[Attribute](new Uid("456", new Name("def456"))))
      .map(_.login) shouldBe Some("abc123")
  }

  test("toObject") {
    DemoSilentModel.toObject((123, "abc123"), QUERY) shouldBe a[Some[_]]
    DemoSilentModel.toObject((-123, "abc123"), QUERY) shouldBe None
    DemoSilentModel.toObject((123, ""), QUERY) shouldBe None
    DemoSilentModel.toObject((123, "abc123"), UPDATE) shouldBe a[Some[_]]
    DemoSilentModel.toObject((-123, "abc123"), UPDATE) shouldBe None
    DemoSilentModel.toObject((123, ""), UPDATE) shouldBe None
    DemoSilentModel.toObject((123, "abc123"), DELETE) shouldBe None
    DemoSilentModel.toObject((123, "abc123"), CREATE) shouldBe None
    DemoSilentModel.toObject((125, "abc125"), QUERY) shouldBe None
    DemoSilentModel.toObject((125, "abc125"), UPDATE) shouldBe None
  }

  test("handle") {
    val handler = new ResultsHandler {
      override def handle(connectorObject: ConnectorObject): Boolean = true
    }
    DemoSilentModel.handle(handler, (123, "abc123"))
    DemoSilentModel.handle(handler, (-123, "abc123"))
    DemoSilentModel.handle(handler, (123, ""))
  }

}
