/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.lisppackage;

import java.util.function.BiConsumer;

import jcl.lang.PackageStruct;
import jcl.lang.SymbolStructImpl;
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
	 * Returns {@link PackageStruct#unexport(SymbolStructImpl[])} as a method reference function.
	 *
	 * @return returns {@link PackageStruct#unexport(SymbolStructImpl[])} as a method reference function
	 */
	@Override
	protected BiConsumer<PackageStruct, SymbolStructImpl[]> symbolListFunction() {
		return PackageStruct::unexport;
	}
}
