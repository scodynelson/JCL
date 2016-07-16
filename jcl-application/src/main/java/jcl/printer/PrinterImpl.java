/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer;

import jcl.lang.LispStruct;
import org.springframework.stereotype.Component;

@Component
public class PrinterImpl implements Printer {

	@Override
	public String print(final LispStruct object) {
		return object.toString();
	}
}
