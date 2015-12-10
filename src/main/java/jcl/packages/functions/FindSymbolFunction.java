/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages.functions;

import java.util.function.BiFunction;

import jcl.packages.PackageStruct;
import jcl.packages.PackageSymbolStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code find-symbol}.
 */
@Component
public final class FindSymbolFunction extends AbstractStringPackageFunction {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 2861315799136211349L;

	/**
	 * Public constructor passing the documentation string.
	 */
	public FindSymbolFunction() {
		super("Locates a symbol whose name is string in a package.");
	}

	@Override
	protected BiFunction<PackageStruct, String, PackageSymbolStruct> packageFunction() {
		return PackageStruct::findSymbol;
	}

	/**
	 * {@inheritDoc}
	 * Returns the function name {@code find-symbol} as a string.
	 *
	 * @return the function name {@code find-symbol} as a string
	 */
	@Override
	protected String functionName() {
		return "FIND-SYMBOL";
	}
}
