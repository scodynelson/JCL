/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer;

import java.io.Serializable;

public interface LispPrinter<O> extends Serializable {

	String print(O object);
}
