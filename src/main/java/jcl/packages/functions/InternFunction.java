/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages.functions;

import java.util.function.BiFunction;

import jcl.packages.PackageStruct;
import jcl.packages.PackageSymbolStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code intern}.
 */
@Component
public final class InternFunction extends AbstractStringPackageFunction {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -4889133141323870879L;

	/**
	 * Public constructor passing the documentation string.
	 */
	public InternFunction() {
		super("Enters a symbol named string into package.");
	}

	@Override
	protected BiFunction<PackageStruct, String, PackageSymbolStruct> packageFunction() {
		return PackageStruct::intern;
	}

	/**
	 * {@inheritDoc}
	 * Returns the function name {@code intern} as a string.
	 *
	 * @return the function name {@code intern} as a string
	 */
	@Override
	protected String functionName() {
		return "INTERN";
	}
}
