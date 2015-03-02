/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl.struct;

import jcl.functions.FunctionStruct;
import jcl.hashtables.HashTableStruct;
import jcl.printer.Printer;
import jcl.printer.impl.LispPrinter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.math.BigInteger;

@Component
public class HashTableStructPrinter implements LispPrinter<HashTableStruct> {

	private static final long serialVersionUID = 8729748390924216544L;

	@Autowired
	private Printer printer;

	@Override
	public String print(final HashTableStruct object) {
		final String typeClassName = object.getType().getClass().getName().toUpperCase();

		final FunctionStruct test = object.getTest();
		final String printedTest = printer.print(test);

		final BigInteger mapSize = object.getCount();

		return "#<" + typeClassName + " :TEST " + printedTest + " size " + mapSize + '>';
	}
}
