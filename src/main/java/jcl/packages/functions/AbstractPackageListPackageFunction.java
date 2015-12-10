/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages.functions;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.function.BiConsumer;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.symbols.TStruct;
import jcl.types.CharacterType;
import jcl.types.ListType;
import jcl.types.PackageType;
import jcl.types.StringType;
import jcl.types.SymbolType;

abstract class AbstractPackageListPackageFunction extends AbstractOptionalPackageFunction {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -5082610749836356431L;

	protected AbstractPackageListPackageFunction(final String documentation) {
		super(documentation);
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return new RequiredParameter.Builder(GlobalPackageStruct.COMMON_LISP, "PACKAGES").buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		validator.validateTypes(lispStruct, functionName(), "Packages", ListType.INSTANCE, StringType.INSTANCE, SymbolType.INSTANCE, CharacterType.INSTANCE, PackageType.INSTANCE);

		final List<PackageStruct> realPackages;
		if (lispStruct instanceof ListStruct) {
			final List<LispStruct> packages = ((ListStruct) lispStruct).getAsJavaList();
			realPackages = new ArrayList<>(packages.size());
			for (final LispStruct aPackage : packages) {
				final PackageStruct realPackage = findPackage(aPackage);
				realPackages.add(realPackage);
			}
		} else {
			final PackageStruct aPackage = findPackage(lispStruct);
			realPackages = Collections.singletonList(aPackage);
		}

		final PackageStruct aPackage = getPackage(lispStructs);
		validatePackages(aPackage);

		final PackageStruct[] realPackageArray = realPackages.toArray(new PackageStruct[realPackages.size()]);
		validatePackages(realPackageArray);

		packageListFunction().accept(aPackage, realPackageArray);

		return TStruct.INSTANCE;
	}

	protected abstract BiConsumer<PackageStruct, PackageStruct[]> packageListFunction();

	protected abstract void validatePackages(PackageStruct... packageStructs);
}
