/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl;

import jcl.lists.NullStruct;
import jcl.printer.LispPrinter;
import org.springframework.stereotype.Component;

@Component
public class NullStructPrinter implements LispPrinter<NullStruct> {

	@Override
	public String print(final NullStruct object) {
		return "NIL";
	}
}
