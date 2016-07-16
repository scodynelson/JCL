/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer;

import jcl.lang.LispStruct;

@FunctionalInterface
public interface Printer {

	String print(LispStruct object);
}
