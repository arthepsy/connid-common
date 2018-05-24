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
import util._

import scala.collection.mutable.ListBuffer

class PagedQuerySpec extends BaseFunSuite {

  class Callback(val maxSize: Int) {
    val pageOffsets: ListBuffer[Int] = ListBuffer[Int]()

    val callback: PagedQuery.Callback[String] =
      new PagedQuery.Callback[String] {
        override def handle(handler: ResultsHandler,
                            pageOffset: Int,
                            pageSize: Int,
                            filter: String): Boolean = {
          pageOffsets += pageOffset
          pageOffset + pageSize < maxSize
        }
      }
  }

  test("paged query without options") {
    val cb = new Callback(40)
    PagedQuery.execute(nullValue, nullValue, nullValue, 10, cb.callback)
    cb.pageOffsets shouldBe Seq(0, 10, 20, 30)
  }

  test("paged query with negative page offset") {
    val cb = new Callback(40)
    val options =
      new OperationOptionsBuilder().setPagedResultsOffset(-1).build()
    PagedQuery.execute(nullValue, options, nullValue, 10, cb.callback)
    cb.pageOffsets shouldBe Seq(0, 10, 20, 30)
  }

  test("paged query with custom page offset") {
    val cb = new Callback(40)
    val options =
      new OperationOptionsBuilder().setPagedResultsOffset(15).build()
    PagedQuery.execute(nullValue, options, nullValue, 10, cb.callback)
    cb.pageOffsets shouldBe Seq(14, 24, 34)
  }

  test("paged query with zero page size") {
    val cb = new Callback(40)
    val options = new OperationOptionsBuilder().setPageSize(0).build()
    PagedQuery.execute(nullValue, options, nullValue, 10, cb.callback)
    cb.pageOffsets shouldBe Seq(0, 10, 20, 30)
  }

  test("paged query with negative page size") {
    val cb = new Callback(40)
    val options = new OperationOptionsBuilder().setPageSize(-1).build()
    PagedQuery.execute(nullValue, options, nullValue, 10, cb.callback)
    cb.pageOffsets shouldBe Seq(0, 10, 20, 30)
  }

  test("paged query with custom page size") {
    val cb = new Callback(40)
    val options = new OperationOptionsBuilder().setPageSize(15).build()
    PagedQuery.execute(nullValue, options, nullValue, 10, cb.callback)
    cb.pageOffsets shouldBe Seq(0, 15, 30)
  }

  test("paged query single page") {
    val cb = new Callback(40)
    val options = new OperationOptionsBuilder()
      .setPagedResultsOffset(15)
      .setPageSize(12)
      .build()
    PagedQuery.execute(nullValue, options, nullValue, 10, cb.callback)
    cb.pageOffsets shouldBe Seq(14)
  }

}
