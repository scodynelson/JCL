/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages.functions;

import java.util.function.BiConsumer;

import jcl.packages.PackageStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code export}.
 */
@Component
public final class ExportFunction extends AbstractSymbolListPackageFunction {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -3271748062551723057L;

	/**
	 * Public constructor passing the documentation string.
	 */
	public ExportFunction() {
		super("Makes one or more symbols that are accessible in package (whether directly or by inheritance) be external symbols of that package.");
	}

	/**
	 * {@inheritDoc}
	 * Returns {@link PackageStruct#export(SymbolStruct[])} as a method reference function.
	 *
	 * @return returns {@link PackageStruct#export(SymbolStruct[])} as a method reference function
	 */
	@Override
	protected BiConsumer<PackageStruct, SymbolStruct[]> symbolListFunction() {
		return PackageStruct::export;
	}

	/**
	 * {@inheritDoc}
	 * Returns the function name {@code export} as a string.
	 *
	 * @return the function name {@code export} as a string
	 */
	@Override
	protected String functionName() {
		return "EXPORT";
	}
}