/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages.functions;

import java.util.function.BiConsumer;

import jcl.packages.PackageStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code import}.
 */
@Component
public final class ImportFunction extends AbstractSymbolListPackageFunction {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -8715208276814574717L;

	/**
	 * Public constructor passing the documentation string.
	 */
	public ImportFunction() {
		super("Adds symbol or symbols to the internals of package, checking for name conflicts with existing symbols either present in package or accessible to it.");
	}

	@Override
	protected BiConsumer<PackageStruct, SymbolStruct<?>[]> symbolListFunction() {
		return PackageStruct::importSymbols;
	}

	/**
	 * {@inheritDoc}
	 * Returns the function name {@code import} as a string.
	 *
	 * @return the function name {@code import} as a string
	 */
	@Override
	protected String functionName() {
		return "IMPORT";
	}
}
