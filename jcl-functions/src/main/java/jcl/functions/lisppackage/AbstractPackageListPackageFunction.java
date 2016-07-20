/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.lisppackage;

import java.util.List;
import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collectors;

import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.PackageStruct;
import jcl.lang.PackageVariables;
import jcl.lang.TStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.FunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

/**
 * Abstract {@link FunctionStruct} implementation for package functions that take package-designator objects. This
 * {@link FunctionStruct} also has an optional package parameter value.
 */
abstract class AbstractPackageListPackageFunction extends CommonLispBuiltInFunctionStruct {

	protected AbstractPackageListPackageFunction(final String documentation, final String functionName) {
		super(documentation, functionName,
		      Parameters.forFunction(functionName)
		                .requiredParameter("PACKAGES")
		                .optionalParameter("PACKAGE").withInitialValue(PackageVariables.PACKAGE.getVariableValue())
		);
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
	public LispStruct apply(final Arguments arguments) {
		final LispStruct lispStruct = arguments.getRequiredArgument("PACKAGES");
		final PackageStruct aPackage = arguments.getOptionalArgument("PACKAGE", PackageStruct.class);
		validatePackages(aPackage);

		final PackageStruct[] realPackageArray;
		if (lispStruct instanceof ListStruct) {
			final ListStruct packages = (ListStruct) lispStruct;
			final List<PackageStruct> realPackages
					= packages.stream()
					          .map(LispStruct::asPackage)
					          .map(Supplier::get)
					          .collect(Collectors.toList());
			realPackageArray = realPackages.toArray(new PackageStruct[realPackages.size()]);
		} else {
			realPackageArray = new PackageStruct[1];
			realPackageArray[0] = lispStruct.asPackage().get();
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