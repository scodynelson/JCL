/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages.functions;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.OptionalParameter;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.functions.FunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.packages.PackageVariables;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;

/**
 * Abstract {@link FunctionStruct} implementation for package functions that operates on an optional {@link
 * PackageStruct} value as the second parameter.
 */
abstract class AbstractOptionalPackageFunction extends AbstractCommonLispFunctionStruct {

	/**
	 * The {@link TypeValidator} for validating the function parameter value types.
	 */
	@Autowired
	protected TypeValidator validator;

	/**
	 * Protected constructor passing the provided {@code documentation} string to the super constructor.
	 *
	 * @param documentation
	 * 		the documentation string
	 */
	protected AbstractOptionalPackageFunction(final String documentation) {
		super(documentation);
	}

	/**
	 * {@inheritDoc}
	 * Creates the single {@link OptionalParameter} package-designator (character, string, symbol, or package) object
	 * for this function.
	 *
	 * @return a list of a single {@link OptionalParameter} package-designator (character, string, symbol, or package)
	 * object
	 */
	@Override
	protected List<OptionalParameter> getOptionalBindings() {
		return OptionalParameter.builder(GlobalPackageStruct.COMMON_LISP, "PACKAGE")
		                        .initForm(PackageVariables.PACKAGE.getVariableValue())
		                        .suppliedPBinding()
		                        .buildList();
	}

	/**
	 * Gets the {@link PackageStruct} from the provided {@link LispStruct} parameters. If there are only 2 parameters,
	 * the {@link PackageStruct} default is the value of {@link PackageVariables#PACKAGE}.
	 *
	 * @param lispStructs
	 * 		the function parameter list
	 *
	 * @return the {@link PackageStruct} from the provided {@link LispStruct} parameters or {@link
	 * PackageVariables#PACKAGE}
	 */
	protected PackageStruct getPackage(final LispStruct... lispStructs) {
		final PackageStruct aPackage;
		if (lispStructs.length >= 2) {
			final LispStruct packageDesignator = lispStructs[1];
			aPackage = validator.validatePackageDesignator(packageDesignator, functionName());
		} else {
			aPackage = PackageVariables.PACKAGE.getVariableValue();
		}
		return aPackage;
	}
}
