/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.lisppackage;

import java.util.function.BiConsumer;

import jcl.lang.PackageStructImpl;
import jcl.lang.SymbolStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code unexport}.
 */
@Component
public final class UnexportFunction extends AbstractSymbolListPackageFunction {

	/**
	 * Public constructor passing the documentation string.
	 */
	public UnexportFunction() {
		super("Reverts external symbols in package to internal status.",
		      "UNEXPORT"
		);
	}

	/**
	 * {@inheritDoc}
	 * Returns {@link PackageStructImpl#unexport(SymbolStruct[])} as a method reference function.
	 *
	 * @return returns {@link PackageStructImpl#unexport(SymbolStruct[])} as a method reference function
	 */
	@Override
	protected BiConsumer<PackageStructImpl, SymbolStruct[]> symbolListFunction() {
		return PackageStructImpl::unexport;
	}
}
