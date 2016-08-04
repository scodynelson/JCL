/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.lisppackage;

import java.util.function.BiConsumer;

import jcl.lang.PackageStruct;
import jcl.lang.SymbolStructImpl;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code shadowing-import}.
 */
@Component
public final class ShadowingImportFunction extends AbstractSymbolListPackageFunction {

	/**
	 * Public constructor passing the documentation string.
	 */
	public ShadowingImportFunction() {
		super("Inserts each of symbols into package as an internal symbol, regardless of whether another symbol of the same name is shadowed by this action.",
		      "SHADOWING-IMPORT"
		);
	}

	/**
	 * {@inheritDoc}
	 * Returns {@link PackageStruct#shadowingImport(SymbolStructImpl[])} as a method reference function.
	 *
	 * @return returns {@link PackageStruct#shadowingImport(SymbolStructImpl[])} as a method reference function
	 */
	@Override
	protected BiConsumer<PackageStruct, SymbolStructImpl[]> symbolListFunction() {
		return PackageStruct::shadowingImport;
	}
}
