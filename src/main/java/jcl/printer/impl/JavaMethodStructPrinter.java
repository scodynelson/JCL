/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl;

import jcl.java.JavaMethodStruct;
import jcl.printer.LispPrinter;
import org.springframework.stereotype.Component;

@Component
public class JavaMethodStructPrinter implements LispPrinter<JavaMethodStruct> {

	@Override
	public String print(final JavaMethodStruct object) {
		return object.getJavaMethod().toString();
	}
}
