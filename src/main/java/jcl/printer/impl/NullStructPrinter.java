/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl;

import jcl.lists.NullStruct;
import jcl.printer.LispPrinter;
import org.springframework.stereotype.Component;

@Component
public class NullStructPrinter implements LispPrinter<NullStruct> {

	private static final long serialVersionUID = -1270182745085221302L;

	@Override
	public String print(final NullStruct object) {
		return "NIL";
	}
}
