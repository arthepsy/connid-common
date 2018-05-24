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

package eu.arthepsy.midpoint.util

import org.identityconnectors.framework.common.objects.{
  OperationOptions,
  ResultsHandler
}

object PagedQuery {
  trait Callback[T] {
    def handle(handler: ResultsHandler,
               pageOffset: Int,
               pageSize: Int,
               filter: T): Boolean
  }
  def execute[T](handler: ResultsHandler,
                 options: OperationOptions,
                 filter: T,
                 defaultPageSize: Int,
                 pagedQueryCallback: Callback[T]): Unit = {
    val pageSize =
      Option(options)
        .flatMap(o => Option(o.getPageSize))
        .map(Predef.Integer2int)
        .filter(p => p > 0)
        .getOrElse(defaultPageSize)
    val pageOffset =
      Option(options)
        .flatMap(o => Option(o.getPagedResultsOffset))
        .map(Predef.Integer2int)
        .filter(p => p >= 1)
        .getOrElse(1) - 1
    val singlePage = Option(options).exists(o =>
      Option(o.getPagedResultsOffset).isDefined && Option(o.getPageSize).isDefined)

    @scala.annotation.tailrec
    def handle(handler: ResultsHandler,
               pageOffset: Int,
               pageSize: Int,
               filter: T): Unit = {
      if (pagedQueryCallback.handle(handler, pageOffset, pageSize, filter) && !singlePage) {
        handle(handler, pageOffset + pageSize, pageSize, filter)
      }
    }
    handle(handler, pageOffset, pageSize, filter)
  }
}
