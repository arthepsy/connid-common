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

import org.identityconnectors.framework.common.exceptions.InvalidAttributeValueException
import org.identityconnectors.framework.common.objects._
import org.mockito.Mockito.{mock, when}

import scala.collection.JavaConverters._
import util.Attributes._

class AttributesSpec extends BaseFunSuite {

  private[this] def mockAttrs(attrs: java.util.Set[Attribute],
                              attr: Attribute,
                              name: String,
                              value: java.util.List[AnyRef]): Unit = {
    when(attr.getName).thenReturn(name)
    when(attr.getValue).thenReturn(value)
    when(attrs.iterator).thenReturn(Seq(attr).toIterator.asJava)
    ()
  }

  test("check attribute existence") {
    val attrs = new java.util.HashSet[Attribute]()
    attrs.add(AttributeBuilder.build("bar1"))
    attrs.add(AttributeBuilder.build("bar2"))
    attrs.add(AttributeBuilder.build("bar3"))
    attrs.hasName("bar0") shouldBe false
    attrs.hasName("bar1") shouldBe true
    attrs.hasName("bar2") shouldBe true
    attrs.hasName("bar3") shouldBe true
    attrs.hasName("bar4") shouldBe false
  }

  test("get attribute value (correct)") {
    val attr = mock(classOf[Attribute])
    when(attr.getName).thenReturn("foo")
    when(attr.getValue).thenReturn(List[AnyRef]("bar").asJava)
    attr.getValue(classOf[String]) shouldBe Some("bar")
  }

  test("get single value of attribute (correct)") {
    val attrs = mock(classOf[java.util.Set[Attribute]])
    val attr = mock(classOf[Attribute])
    this.mockAttrs(attrs, attr, "foo", List[AnyRef]("bar").asJava)
    attrs.getValue("foo", classOf[String]) shouldBe Some("bar")
  }

  test("get single value of attribute (null)") {
    val attrs = mock(classOf[java.util.Set[Attribute]])
    val attr = mock(classOf[Attribute])
    this.mockAttrs(attrs, attr, nullValue, nullValue)
    attrs.getValue("foo", classOf[String]) shouldBe None
    this.mockAttrs(attrs, attr, "foo", nullValue)
    attrs.getValue("foo", classOf[String]) shouldBe None
  }

  test("get single value of attribute (empty)") {
    val attrs = mock(classOf[java.util.Set[Attribute]])
    val attr = mock(classOf[Attribute])
    this.mockAttrs(attrs, attr, "foo", List[AnyRef]().asJava)
    attrs.getValue("foo", classOf[String]) shouldBe None
    this.mockAttrs(attrs, attr, "foo", List[AnyRef](nullValue).asJava)
    attrs.getValue("foo", classOf[String]) shouldBe None
  }

  test("get single value of attribute (invalid type)") {
    val attrs = mock(classOf[java.util.Set[Attribute]])
    val attr = mock(classOf[Attribute])
    this.mockAttrs(attrs, attr, "foo", List[AnyRef](new Integer(123)).asJava)
    assertThrows[InvalidAttributeValueException] {
      attrs.getValue("foo", classOf[String]) shouldBe "123"
    }
  }

  test("get attribute mutli values (correct)") {
    val attr = mock(classOf[Attribute])
    when(attr.getName).thenReturn("foo")
    when(attr.getValue).thenReturn(List[AnyRef]("bar1", "bar2").asJava)
    attr.getValues(classOf[String]) shouldBe Seq("bar1", "bar2")
  }

  test("get single value of attribute (multiple)") {
    val attrs = mock(classOf[java.util.Set[Attribute]])
    val attr = mock(classOf[Attribute])
    this.mockAttrs(attrs, attr, "foo", List[AnyRef]("bar1", "bar2").asJava)
    assertThrows[InvalidAttributeValueException] {
      attrs.getValue("foo", classOf[String]) shouldBe "bar1"
    }
  }

  test("get multi value of attribute (correct)") {
    val attrs = mock(classOf[java.util.Set[Attribute]])
    val attr = mock(classOf[Attribute])
    this.mockAttrs(attrs, attr, "foo", List[AnyRef]("foo", "bar").asJava)
    attrs.getValues("foo", classOf[String]) shouldBe Seq("foo", "bar")
  }

  test("get multi value of attribute (null)") {
    val attrs = mock(classOf[java.util.Set[Attribute]])
    val attr = mock(classOf[Attribute])
    this.mockAttrs(attrs, attr, nullValue, nullValue)
    attrs.getValues("foo", classOf[String]) shouldBe Seq.empty
    this.mockAttrs(attrs, attr, "foo", nullValue)
    attrs.getValues("foo", classOf[String]) shouldBe Seq.empty
  }

  test("get multi value of attribute (empty)") {
    val attrs = mock(classOf[java.util.Set[Attribute]])
    val attr = mock(classOf[Attribute])
    this.mockAttrs(attrs, attr, "foo", List[AnyRef]().asJava)
    attrs.getValues("foo", classOf[String]) shouldBe Seq.empty
    this.mockAttrs(attrs, attr, "foo", List[AnyRef](nullValue).asJava)
    attrs.getValues("foo", classOf[String]) shouldBe Seq.empty
  }

  test("get multi value of attribute (invalid type)") {
    val attrs = mock(classOf[java.util.Set[Attribute]])
    val attr = mock(classOf[Attribute])
    this.mockAttrs(attrs,
                   attr,
                   "foo",
                   List[AnyRef](Int.box(123), Int.box(456)).asJava)
    assertThrows[InvalidAttributeValueException] {
      attrs.getValues("foo", classOf[String]) shouldBe Seq("123", "456")
    }
  }

  test("get multi value of attribute (single)") {
    val attrs = mock(classOf[java.util.Set[Attribute]])
    val attr = mock(classOf[Attribute])
    this.mockAttrs(attrs, attr, "foo", List[AnyRef]("bar1").asJava)
    attrs.getValues("foo", classOf[String]) shouldBe Seq("bar1")
  }

}
