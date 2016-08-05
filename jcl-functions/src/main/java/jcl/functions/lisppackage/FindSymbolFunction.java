/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.lisppackage;

import java.util.function.BiFunction;

import jcl.lang.PackageStructImpl;
import jcl.lang.PackageSymbolStruct;
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
		super("Locates a symbol whose name is string in a package.",
		      "FIND-SYMBOL"
		);
	}

	/**
	 * {@inheritDoc}
	 * Returns {@link PackageStructImpl#findSymbol(String)} as a method reference function.
	 *
	 * @return returns {@link PackageStructImpl#findSymbol(String)} as a method reference function
	 */
	@Override
	protected BiFunction<PackageStructImpl, String, PackageSymbolStruct> packageFunction() {
		return PackageStructImpl::findSymbol;
	}
}
