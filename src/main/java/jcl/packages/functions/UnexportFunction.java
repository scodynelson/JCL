/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages.functions;

import java.util.function.BiConsumer;

import jcl.packages.PackageStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code unexport}.
 */
@Component
public final class UnexportFunction extends AbstractSymbolListPackageFunction {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -6515511500131565347L;

	/**
	 * Public constructor passing the documentation string.
	 */
	public UnexportFunction() {
		super("Reverts external symbols in package to internal status.");
	}

	/**
	 * {@inheritDoc}
	 * Returns {@link PackageStruct#unexport(SymbolStruct[])} as a method reference function.
	 *
	 * @return returns {@link PackageStruct#unexport(SymbolStruct[])} as a method reference function
	 */
	@Override
	protected BiConsumer<PackageStruct, SymbolStruct<?>[]> symbolListFunction() {
		return PackageStruct::unexport;
	}

	/**
	 * {@inheritDoc}
	 * Returns the function name {@code unexport} as a string.
	 *
	 * @return the function name {@code unexport} as a string
	 */
	@Override
	protected String functionName() {
		return "UNEXPORT";
	}
}
