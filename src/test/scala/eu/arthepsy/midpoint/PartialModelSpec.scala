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

import java.util

import org.identityconnectors.framework.common.objects.Attribute
import org.scalatest.PrivateMethodTester

class PartialModelSpec extends BaseFunSuite with PrivateMethodTester {

  class PartialModelTest extends PartialModel[String] {
    override def isValidFor(op: Model.OP): Boolean = ???
    override def toAttributes(op: Model.OP): Option[util.Set[Attribute]] = ???
    override def toConnectorAttributes(op: Model.OP): Set[Attribute] = ???
    override def toNative(op: Model.OP): Option[String] = ???

    def testIsBlank(x: String): Boolean = isBlank(x)
    def testIsBlank(x: Option[String]): Boolean = isBlank(x)

  }

  test("isBlank") {
    val pm = new PartialModelTest
    pm.testIsBlank("") shouldBe true
    pm.testIsBlank(" ") shouldBe true
    pm.testIsBlank("   ") shouldBe true
    pm.testIsBlank("\t ") shouldBe true
    pm.testIsBlank(" \t") shouldBe true
    pm.testIsBlank(" \t ") shouldBe true
    val v: String = nullValue
    pm.testIsBlank(v) shouldBe true
    pm.testIsBlank(None) shouldBe true
    pm.testIsBlank(Some("")) shouldBe true
    pm.testIsBlank(Some(" ")) shouldBe true
    pm.testIsBlank(Some("   ")) shouldBe true
    pm.testIsBlank(Some("\t ")) shouldBe true
    pm.testIsBlank(Some(" \t")) shouldBe true
    pm.testIsBlank(Some(" \t ")) shouldBe true
    pm.testIsBlank("foo") shouldBe false
  }
}
