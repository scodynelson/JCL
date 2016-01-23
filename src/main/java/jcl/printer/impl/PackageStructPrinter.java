/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl;

import jcl.packages.PackageStruct;
import jcl.printer.LispPrinter;
import org.springframework.stereotype.Component;

@Component
public class PackageStructPrinter implements LispPrinter<PackageStruct> {

	@Override
	public String print(final PackageStruct object) {
		final String typeClassName = object.getType().getClass().getSimpleName().toUpperCase();
		final String packageName = object.getName();
		return "#<PACKAGE \"" + packageName + "\">";
	}
}
