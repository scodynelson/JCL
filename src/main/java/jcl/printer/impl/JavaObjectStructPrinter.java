/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl;

import jcl.java.JavaObjectStruct;
import jcl.printer.LispPrinter;
import org.springframework.stereotype.Component;

@Component
public class JavaObjectStructPrinter implements LispPrinter<JavaObjectStruct> {

	@Override
	public String print(final JavaObjectStruct object) {
		return object.getJavaObject().toString();
	}
}
