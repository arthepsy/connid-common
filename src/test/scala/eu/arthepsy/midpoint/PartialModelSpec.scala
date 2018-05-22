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

import org.identityconnectors.framework.common.objects.{
  Attribute,
  AttributeInfo
}

class PartialModelSpec extends BaseFunSuite {

  class PartialModelTest extends PartialModel[String] {
    override def isValidFor(op: Model.OP): Boolean = false
    override def toAttributes(op: Model.OP): Option[Set[Attribute]] = None
    override def toNative(op: Model.OP): Option[String] = None
  }

  object PartialModelTest
      extends PartialModel.Object[String, PartialModelTest] {
    override def attrNames: Seq[String] = Seq.empty
    override def attrInfos: Seq[AttributeInfo] = Seq.empty
    override def parse(native: String): Option[PartialModelTest] = None
    override def parse(set: Set[Attribute]): Option[PartialModelTest] = None
  }

  test("java to scala set") {
    import org.mockito.Mockito._
    import org.mockito.ArgumentMatchers._
    val tm = spy(PartialModelTest)
    doCallRealMethod().when(tm).parse(any[java.util.Set[Attribute]])
    doReturn(None, None).when(tm).parse(any[Set[Attribute]]())
    tm.parse(new java.util.HashSet[Attribute]())
    tm.parse(nullValue: java.util.Set[Attribute])
    verify(tm, times(2)).parse(any[java.util.Set[Attribute]])
    verify(tm, times(1)).parse(any[Set[Attribute]])
    verifyNoMoreInteractions(tm)
  }
}
