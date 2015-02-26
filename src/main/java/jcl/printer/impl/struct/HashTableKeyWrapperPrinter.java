/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl.struct;

import jcl.LispStruct;
import jcl.hashtables.HashTableStruct;
import jcl.printer.Printer;
import jcl.printer.impl.LispPrinter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class HashTableKeyWrapperPrinter implements LispPrinter<HashTableStruct.KeyWrapper> {

	@Autowired
	private Printer printer;

	@Override
	public String print(final HashTableStruct.KeyWrapper object) {
		final LispStruct key = object.getKey();
		return printer.print(key);
	}
}
