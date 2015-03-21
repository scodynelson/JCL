/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl;

import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.packages.PackageSymbolStruct;
import jcl.packages.PackageVariables;
import jcl.printer.LispPrinter;
import jcl.printer.PrinterVariables;
import jcl.symbols.BooleanStruct;
import jcl.symbols.SymbolStruct;
import jcl.system.CommonLispSymbols;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.stereotype.Component;

@Component
public class SymbolStructPrinter implements LispPrinter<SymbolStruct<?>> {

	private static final long serialVersionUID = 5098070113503702856L;

	@Override
	public String print(final SymbolStruct<?> object) {
		final BooleanStruct printEscape = PrinterVariables.PRINT_ESCAPE.getValue();

		// TODO: deal with *PRINT-CASE* and *PRINT-ESCAPE*

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

			final boolean externalSymbol = CommonLispSymbols.EXTERNAL.equals(symbol.getPackageSymbolType());
			if (externalSymbol) {
				// TODO: verify it is a single colon for external symbols when printing...
				return packageName + ':' + name;
			} else {
				return packageName + "::" + name;
			}
		}
		return name;
	}

	@Override
	@SuppressWarnings("checkstyle:strictduplicatecodecheck")
	public int hashCode() {
		return HashCodeBuilder.reflectionHashCode(this);
	}

	@Override
	@SuppressWarnings("checkstyle:strictduplicatecodecheck")
	public boolean equals(final Object obj) {
		return EqualsBuilder.reflectionEquals(this, obj);
	}

	@Override
	@SuppressWarnings("checkstyle:strictduplicatecodecheck")
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).toString();
	}
}
