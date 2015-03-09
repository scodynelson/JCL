/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer;

import java.io.Serializable;

import jcl.LispStruct;

public interface Printer extends Serializable {

	String print(LispStruct object);
}
