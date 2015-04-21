/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl;

import jcl.java.JavaMethodStruct;
import jcl.printer.LispPrinter;
import org.springframework.stereotype.Component;

@Component
public class JavaMethodStructPrinter implements LispPrinter<JavaMethodStruct> {

	private static final long serialVersionUID = -9138697167024221687L;

	@Override
	public String print(final JavaMethodStruct object) {
		return object.getJavaMethod().toString();
	}
}
