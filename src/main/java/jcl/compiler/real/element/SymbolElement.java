/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element;

import jcl.LispStruct;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.packages.PackageSymbolStruct;
import jcl.symbols.KeywordSymbolStruct;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.util.Map;

public class SymbolElement implements SimpleElement {

	private static final long serialVersionUID = 6848146265489259786L;

	private final String packageName;

	private final String symbolName;

	private final boolean isExternalSymbol;

	public SymbolElement(final String packageName, final String symbolName) {
		this(packageName, symbolName, false);
	}

	public SymbolElement(final String packageName, final String symbolName, final boolean isExternalSymbol) {
		this.packageName = packageName;
		this.symbolName = symbolName;
		this.isExternalSymbol = isExternalSymbol;
	}

	public String getPackageName() {
		return packageName;
	}

	public String getSymbolName() {
		return symbolName;
	}

	public boolean isExternalSymbol() {
		return isExternalSymbol;
	}

	@Override
	public LispStruct toLispStruct() {
		final PackageStruct pkg = PackageStruct.findPackage(packageName);

		if (pkg == null) {
			throw new RuntimeException("There is no package named " + packageName);
		}

		if (GlobalPackageStruct.KEYWORD.equals(pkg)) {
			final PackageSymbolStruct pkgSymStruct = GlobalPackageStruct.KEYWORD.findSymbol(symbolName);
			return (pkgSymStruct == null) ? new KeywordSymbolStruct(symbolName) : pkgSymStruct.getSymbolStruct();
		}

		if (isExternalSymbol) {
			final Map<String, SymbolStruct<?>> pkgExternalSymbols = pkg.getExternalSymbols();

			final SymbolStruct<?> externalSymbol = pkgExternalSymbols.get(symbolName);
			if (externalSymbol == null) {
				throw new ReaderErrorException("No external symbol named \"" + symbolName + "\" in package " + packageName);
			}
			return externalSymbol;
		} else {
			final PackageSymbolStruct packageSymbol = pkg.findSymbol(symbolName);

			final SymbolStruct<?> symbol = packageSymbol.getSymbolStruct();
			if (symbol == null) {
				throw new ReaderErrorException("Unbound variable: " + packageName + "::" + symbolName);
			}
			return symbol;
		}
	}

	@Override
	public int hashCode() {
		return HashCodeBuilder.reflectionHashCode(this);
	}

	@Override
	public boolean equals(final Object obj) {
		return EqualsBuilder.reflectionEquals(this, obj);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
