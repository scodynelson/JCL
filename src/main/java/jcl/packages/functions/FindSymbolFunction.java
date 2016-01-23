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
	 * Public constructor passing the documentation string.
	 */
	public FindSymbolFunction() {
		super("Locates a symbol whose name is string in a package.");
	}

	/**
	 * {@inheritDoc}
	 * Returns {@link PackageStruct#findSymbol(String)} as a method reference function.
	 *
	 * @return returns {@link PackageStruct#findSymbol(String)} as a method reference function
	 */
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
