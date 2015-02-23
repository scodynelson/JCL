/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element;

import jcl.LispStruct;

public interface SimpleElement extends Element {

	LispStruct toLispStruct();
}
