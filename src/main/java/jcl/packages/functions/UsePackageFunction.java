/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages.functions;

import java.util.ArrayList;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.conditions.exceptions.ErrorException;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.symbols.TStruct;
import jcl.types.ListType;
import org.springframework.stereotype.Component;

@Component
public final class UsePackageFunction extends AbstractOptionalPackageFunction {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -2314927832245574872L;

	/**
	 * Public constructor passing the documentation string.
	 */
	public UsePackageFunction() {
		super("Causes package to inherit all the external symbols of packages-to-use.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return new RequiredParameter.Builder(GlobalPackageStruct.COMMON_LISP, "PACKAGES-TO-USE").buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		validator.validateTypes(lispStruct, functionName(), "Packages To Use", ListType.INSTANCE);

		final List<LispStruct> packages = ((ListStruct) lispStruct).getAsJavaList();
		final List<PackageStruct> realPackages = new ArrayList<>(packages.size());
		for (final LispStruct aPackage : packages) {
			final PackageStruct realPackage = findPackage(aPackage);
			realPackages.add(realPackage);
		}

		final PackageStruct aPackage = getPackage(lispStructs);
		validateNotKeywordPackage(aPackage);

		final PackageStruct[] packageArray = realPackages.toArray(new PackageStruct[realPackages.size()]);
		validateNotKeywordPackage(packageArray);

		aPackage.usePackage(packageArray);
		return TStruct.INSTANCE;
	}

	private static void validateNotKeywordPackage(final PackageStruct... packageStructs) {
		for (final PackageStruct packageStruct : packageStructs) {
			if (GlobalPackageStruct.KEYWORD.equals(packageStruct)) {
				throw new ErrorException("Cannot use KEYWORD Package.");
			}
		}
	}

	@Override
	protected String functionName() {
		return "USE-PACKAGE";
	}
}
