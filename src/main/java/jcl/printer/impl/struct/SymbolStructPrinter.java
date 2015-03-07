/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl.struct;

import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.packages.PackageSymbolStruct;
import jcl.packages.PackageVariables;
import jcl.printer.impl.SymbolPrinter;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public class SymbolStructPrinter extends SymbolPrinter<SymbolStruct<?>> {

	private static final long serialVersionUID = 5098070113503702856L;

	@Override
	protected String getName(final SymbolStruct<?> object) {
		final PackageStruct pkg = object.getSymbolPackage();
		final String name = object.getName();

		if (pkg == null) {
			return "#:" + name;
		}

		// TODO: look into symbols with '|x| pattern...

		if (GlobalPackageStruct.KEYWORD.equals(pkg)) {
			return ':' + name;
		}

		// TODO: the following isn't right. It's more like the symbol is not "accessible" in the current package...
		// TODO: probably by use of 'findSymbol'

		final PackageStruct currentPackage = PackageVariables.PACKAGE.getValue();

		PackageSymbolStruct symbol = currentPackage.findSymbol(name);
		if (symbol == null) {
			symbol = pkg.findSymbol(name);

			final String packageName = pkg.getName();

			final boolean externalSymbol = PackageSymbolStruct.EXTERNAL.equals(symbol.getPackageSymbolType());
			if (externalSymbol) {
				// TODO: verify it is a single colon for external symbols when printing...
				return packageName + ':' + name;
			} else {
				return packageName + "::" + name;
			}
		}
		return name;
	}
}
