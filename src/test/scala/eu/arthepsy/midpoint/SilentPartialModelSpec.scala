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

class SilentPartialModelSpec extends BaseFunSuite {

  class SilentPartialDemoModel(val id: Int, val login: String)
      extends silent.PartialModel[(Int, String)] {

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

  object SilentPartialDemoModel
      extends silent.PartialModel.Object[SilentPartialDemoModel, (Int, String)] {
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
        native: (Int, String)): Option[SilentPartialDemoModel] = {
      native match {
        case (id, login) if id >= 0 && !util.isBlank(login) =>
          Option(new SilentPartialDemoModel(id, login))
        case (_, login) if !util.isBlank(login) =>
          None
        case _ => None
      }
    }

    override def parse(set: Set[Attribute]): Option[SilentPartialDemoModel] = {
      (set.getValue(Uid.NAME, classOf[String]).map(_.toInt),
       set.getValue(Name.NAME, classOf[String])) match {
        case (Some(id), Some(login)) if id >= 0 && !util.isBlank(login) =>
          Option(new SilentPartialDemoModel(id, login))
        case (_, Some(login)) if util.isBlank(login) =>
          None
        case _ => None
      }
    }

  }

  test("parse java attributes") {
    SilentPartialDemoModel
      .parse(nullValue: java.util.Set[Attribute])
      .map(_.id) shouldBe None
    SilentPartialDemoModel
      .parse(new java.util.HashSet[Attribute](
        Seq(new Uid("123"), new Name("abc123")).asJavaCollection))
      .map(_.id) shouldBe Some(123)
  }

}
