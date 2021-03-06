/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer;

import jcl.lang.LispStruct;
import lombok.experimental.UtilityClass;

@UtilityClass
public final class Printer {

	public String print(final LispStruct object) {
		return object.toString();
	}
}
