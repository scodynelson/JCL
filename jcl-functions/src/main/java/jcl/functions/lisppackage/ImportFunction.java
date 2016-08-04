/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.lisppackage;

import java.util.function.BiConsumer;

import jcl.lang.PackageStruct;
import jcl.lang.SymbolStructImpl;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code import}.
 */
@Component
public final class ImportFunction extends AbstractSymbolListPackageFunction {

	/**
	 * Public constructor passing the documentation string.
	 */
	public ImportFunction() {
		super("Adds symbol or symbols to the internals of package, checking for name conflicts with existing symbols either present in package or accessible to it.",
		      "IMPORT"
		);
	}

	/**
	 * {@inheritDoc}
	 * Returns {@link PackageStruct#importSymbols(SymbolStructImpl[])} as a method reference function.
	 *
	 * @return returns {@link PackageStruct#importSymbols(SymbolStructImpl[])} as a method reference function
	 */
	@Override
	protected BiConsumer<PackageStruct, SymbolStructImpl[]> symbolListFunction() {
		return PackageStruct::importSymbols;
	}
}
