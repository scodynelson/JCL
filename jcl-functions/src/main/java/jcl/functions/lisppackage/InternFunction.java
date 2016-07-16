/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.lisppackage;

import java.util.function.BiFunction;

import jcl.lang.PackageStruct;
import jcl.lang.PackageSymbolStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code intern}.
 */
@Component
public final class InternFunction extends AbstractStringPackageFunction {

	/**
	 * Public constructor passing the documentation string.
	 */
	public InternFunction() {
		super("Enters a symbol named string into package.",
		      "INTERN"
		);
	}

	/**
	 * {@inheritDoc}
	 * Returns {@link PackageStruct#intern(String)} as a method reference function.
	 *
	 * @return returns {@link PackageStruct#intern(String)} as a method reference function
	 */
	@Override
	protected BiFunction<PackageStruct, String, PackageSymbolStruct> packageFunction() {
		return PackageStruct::intern;
	}
}
