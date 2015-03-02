/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl.struct;

import jcl.packages.PackageStruct;
import jcl.printer.impl.LispPrinter;
import org.springframework.stereotype.Component;

@Component
public class PackageStructPrinter implements LispPrinter<PackageStruct> {

	private static final long serialVersionUID = -7212596989249677180L;

	@Override
	public String print(final PackageStruct object) {
		final String typeClassName = object.getType().getClass().getName().toUpperCase();
		final String packageName = object.getName();
		return "#<" + typeClassName + ' ' + packageName + '>';
	}
}
