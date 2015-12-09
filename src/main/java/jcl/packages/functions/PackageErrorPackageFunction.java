/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages.functions;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.conditions.ConditionType;
import jcl.conditions.exceptions.PackageErrorException;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code package-error-package}.
 */
@Component
public final class PackageErrorPackageFunction extends AbstractCommonLispFunctionStruct {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -1020498521237693044L;

	/**
	 * The {@link TypeValidator} for validating the function parameter value types.
	 */
	@Autowired
	private TypeValidator validator;

	/**
	 * Public constructor passing the documentation string.
	 */
	public PackageErrorPackageFunction() {
		super("Returns a designator for the offending package in the situation represented by the condition.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return new RequiredParameter.Builder(GlobalPackageStruct.COMMON_LISP, "CONDITION").buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		validator.validateTypes(lispStruct, functionName(), "Package Error", ConditionType.PACKAGE_ERROR);

		final PackageErrorException packageErrorException = (PackageErrorException) lispStruct;
		return packageErrorException.getPackageWithError();
	}

	/**
	 * {@inheritDoc}
	 * Returns the function name {@code package-error-package} as a string.
	 *
	 * @return the function name {@code package-error-package} as a string
	 */
	@Override
	protected String functionName() {
		return "PACKAGE-ERROR-PACKAGE";
	}
}
