/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages.functions;

import java.util.List;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.lists.NullStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code package-name}.
 */
@Component
public final class PackageNameFunction extends AbstractPackageFunction {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 3957544783757717992L;

	/**
	 * Public constructor passing the documentation string.
	 */
	public PackageNameFunction() {
		super("Returns the string that names package.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return new RequiredParameter.Builder(GlobalPackageStruct.COMMON_LISP, "PACKAGE").buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		final PackageStruct aPackage = findPackage(lispStruct);

		final String name = aPackage.getName();
		return (name == null) ? NullStruct.INSTANCE : new StringStruct(name);
	}

	/**
	 * {@inheritDoc}
	 * Returns the function name {@code package-name} as a string.
	 *
	 * @return the function name {@code package-name} as a string
	 */
	@Override
	protected String functionName() {
		return "PACKAGE-NAME";
	}
}
