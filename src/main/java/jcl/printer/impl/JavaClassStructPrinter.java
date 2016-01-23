/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl;

import jcl.java.JavaClassStruct;
import jcl.printer.LispPrinter;
import org.springframework.stereotype.Component;

@Component
public class JavaClassStructPrinter implements LispPrinter<JavaClassStruct> {

	@Override
	public String print(final JavaClassStruct object) {
		return object.getJavaClass().toString();
	}
}
