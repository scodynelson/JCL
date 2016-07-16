/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages.functions;

import java.util.function.BiConsumer;

import jcl.lang.PackageStruct;
import jcl.lang.SymbolStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code export}.
 */
@Component
public final class ExportFunction extends AbstractSymbolListPackageFunction {

	/**
	 * Public constructor passing the documentation string.
	 */
	public ExportFunction() {
		super("Makes one or more symbols that are accessible in package (whether directly or by inheritance) be external symbols of that package.",
		      "EXPORT"
		);
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
}
