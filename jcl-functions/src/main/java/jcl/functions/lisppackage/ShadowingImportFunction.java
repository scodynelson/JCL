/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.lisppackage;

import java.util.function.BiConsumer;

import jcl.lang.PackageStructImpl;
import jcl.lang.SymbolStruct;
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
	 * Returns {@link PackageStructImpl#shadowingImport(SymbolStruct[])} as a method reference function.
	 *
	 * @return returns {@link PackageStructImpl#shadowingImport(SymbolStruct[])} as a method reference function
	 */
	@Override
	protected BiConsumer<PackageStructImpl, SymbolStruct[]> symbolListFunction() {
		return PackageStructImpl::shadowingImport;
	}
}
