/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer;

@FunctionalInterface
public interface LispPrinter<O> {

	String print(O object);
}
