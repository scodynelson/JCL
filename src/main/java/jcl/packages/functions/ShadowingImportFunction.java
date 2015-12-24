/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages.functions;

import java.util.function.BiConsumer;

import jcl.packages.PackageStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code shadowing-import}.
 */
@Component
public final class ShadowingImportFunction extends AbstractSymbolListPackageFunction {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 2262630739735499759L;

	/**
	 * Public constructor passing the documentation string.
	 */
	public ShadowingImportFunction() {
		super("Inserts each of symbols into package as an internal symbol, regardless of whether another symbol of the same name is shadowed by this action.");
	}

	/**
	 * {@inheritDoc}
	 * Returns {@link PackageStruct#shadowingImport(SymbolStruct[])} as a method reference function.
	 *
	 * @return returns {@link PackageStruct#shadowingImport(SymbolStruct[])} as a method reference function
	 */
	@Override
	protected BiConsumer<PackageStruct, SymbolStruct[]> symbolListFunction() {
		return PackageStruct::shadowingImport;
	}

	/**
	 * {@inheritDoc}
	 * Returns the function name {@code shadowing-import} as a string.
	 *
	 * @return the function name {@code shadowing-import} as a string
	 */
	@Override
	protected String functionName() {
		return "SHADOWING-IMPORT";
	}
}
