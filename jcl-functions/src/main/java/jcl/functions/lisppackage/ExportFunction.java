/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.lisppackage;

import java.util.function.BiConsumer;

import jcl.lang.PackageStructImpl;
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
	 * Returns {@link PackageStructImpl#export(SymbolStruct[])} as a method reference function.
	 *
	 * @return returns {@link PackageStructImpl#export(SymbolStruct[])} as a method reference function
	 */
	@Override
	protected BiConsumer<PackageStructImpl, SymbolStruct[]> symbolListFunction() {
		return PackageStructImpl::export;
	}
}
