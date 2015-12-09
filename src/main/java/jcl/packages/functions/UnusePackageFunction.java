/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages.functions;

import java.util.ArrayList;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.symbols.TStruct;
import jcl.types.ListType;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code unuse-package}.
 */
@Component
public final class UnusePackageFunction extends AbstractOptionalPackageFunction {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 8241215607658705107L;

	/**
	 * Public constructor passing the documentation string.
	 */
	public UnusePackageFunction() {
		super("Causes package to cease inheriting all the external symbols of packages-to-unuse.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return new RequiredParameter.Builder(GlobalPackageStruct.COMMON_LISP, "PACKAGES-TO-UNUSE").buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		validator.validateTypes(lispStruct, functionName(), "Packages To Unuse", ListType.INSTANCE);

		final List<LispStruct> packages = ((ListStruct) lispStruct).getAsJavaList();
		final List<PackageStruct> realPackages = new ArrayList<>(packages.size());
		for (final LispStruct aPackage : packages) {
			final PackageStruct realPackage = findPackage(aPackage);
			realPackages.add(realPackage);
		}

		final PackageStruct aPackage = getPackage(lispStructs);
		final PackageStruct[] packageArray = realPackages.toArray(new PackageStruct[realPackages.size()]);
		aPackage.unUsePackage(packageArray);

		return TStruct.INSTANCE;
	}

	/**
	 * {@inheritDoc}
	 * Returns the function name {@code unuse-package} as a string.
	 *
	 * @return the function name {@code unuse-package} as a string
	 */
	@Override
	protected String functionName() {
		return "UNUSE-PACKAGE";
	}
}
