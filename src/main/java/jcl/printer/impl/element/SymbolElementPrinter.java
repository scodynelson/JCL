/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl.element;

import jcl.compiler.real.element.SymbolElement;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.packages.PackageSymbolStruct;
import jcl.packages.PackageVariables;
import jcl.printer.impl.SymbolPrinter;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

@Component
public class SymbolElementPrinter extends SymbolPrinter<SymbolElement> {

	@Override
	protected String getName(final SymbolElement object) {
		final String packageName = object.getPackageName();
		final String symbolName = object.getSymbolName();

		// TODO: look into symbols with '|x| pattern...

		if (StringUtils.equalsIgnoreCase(GlobalPackageStruct.KEYWORD.getName(), packageName)) {
			return ':' + symbolName;
		}

		// TODO: the following isn't right. It's more like the symbol is not "accessible" in the current package...
		// TODO: probably by use of 'findSymbol'

		final PackageStruct currentPackage = PackageVariables.PACKAGE.getValue();
		final String currentPackageName = currentPackage.getName();

		final PackageSymbolStruct symbol = currentPackage.findSymbol(symbolName);
		if (symbol == null) {

			if (!currentPackageName.equals(packageName)) {
				final boolean externalSymbol = object.isExternalSymbol();
				if (externalSymbol) {
					// TODO: verify it is a single colon for external symbols when printing...
					return packageName + ':' + symbolName;
				} else {
					return packageName + "::" + symbolName;
				}
			}
		}
		return symbolName;
	}
}
