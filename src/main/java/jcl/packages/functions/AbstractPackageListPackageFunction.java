/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages.functions;

import java.util.List;
import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.stream.Collectors;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.FunctionStruct;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.symbols.TStruct;
import jcl.types.CharacterType;
import jcl.types.ListType;
import jcl.types.PackageType;
import jcl.types.StringType;
import jcl.types.SymbolType;

/**
 * Abstract {@link FunctionStruct} implementation for package functions that take package-designator objects. This
 * {@link FunctionStruct} also has an optional package parameter value.
 */
abstract class AbstractPackageListPackageFunction extends AbstractOptionalPackageFunction {

	/**
	 * Protected constructor passing the provided {@code documentation} string to the super constructor.
	 *
	 * @param documentation
	 * 		the documentation string
	 */
	protected AbstractPackageListPackageFunction(final String documentation) {
		super(documentation);
	}

	/**
	 * {@inheritDoc}
	 * Creates the single {@link RequiredParameter} package-designator (character, string, symbol, or package) list
	 * object for this function.
	 *
	 * @return a list of a single {@link RequiredParameter} package-designator (character, string, symbol, or package)
	 * list object
	 */
	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "PACKAGES").buildList();
	}

	/**
	 * Application method for the package function that gets the package(s) from the package-designators, validates the
	 * objects are indeed {@link PackageStruct}s, and applies the {@link Function} from the {@link
	 * #packageListFunction()}.
	 *
	 * @param lispStructs
	 * 		the function parameters
	 *
	 * @return {@link TStruct#INSTANCE}
	 */
	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		validator.validateTypes(lispStruct, functionName(), "Packages", ListType.INSTANCE, StringType.INSTANCE, SymbolType.INSTANCE, CharacterType.INSTANCE, PackageType.INSTANCE);

		final PackageStruct aPackage = getPackage(lispStructs);
		validatePackages(aPackage);

		final PackageStruct[] realPackageArray;
		if (lispStruct instanceof ListStruct) {
			final List<LispStruct> packages = ((ListStruct) lispStruct).getAsJavaList();
			final List<PackageStruct> realPackages
					= packages.stream()
					          .map(e -> validator.validatePackageDesignator(e, functionName()))
					          .collect(Collectors.toList());
			realPackageArray = realPackages.toArray(new PackageStruct[realPackages.size()]);
		} else {
			realPackageArray = new PackageStruct[1];
			realPackageArray[0] = validator.validatePackageDesignator(lispStruct, functionName());
		}

		validatePackages(realPackageArray);
		packageListFunction().accept(aPackage, realPackageArray);

		return TStruct.INSTANCE;
	}

	/**
	 * Abstract method to return a {@link BiConsumer} function that consumes a {@link PackageStruct} and a separate
	 * array of {@link PackageStruct}s.
	 *
	 * @return a {@link BiConsumer} function that consumes a {@link PackageStruct} and a separate array of {@link
	 * PackageStruct}s
	 */
	protected abstract BiConsumer<PackageStruct, PackageStruct[]> packageListFunction();

	/**
	 * Abstract method to validate the provided {@link PackageStruct}s to ensure they meet the function parameter
	 * criteria.
	 *
	 * @param packageStructs
	 * 		the {@link PackageStruct}s to validate
	 */
	protected abstract void validatePackages(PackageStruct... packageStructs);
}
