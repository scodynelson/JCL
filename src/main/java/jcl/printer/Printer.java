/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer;

import jcl.LispStruct;
import jcl.compiler.real.element.Element;

import java.io.Serializable;

public interface Printer extends Serializable {

	String print(LispStruct object);

	String print(Element object);
}
