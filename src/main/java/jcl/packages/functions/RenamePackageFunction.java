/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages.functions;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.OptionalParameter;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.types.ListType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code rename-package}.
 */
@Component
public final class RenamePackageFunction extends AbstractCommonLispFunctionStruct {

	/**
	 * The {@link TypeValidator} for validating the function parameter value types.
	 */
	@Autowired
	private TypeValidator validator;

	/**
	 * Public constructor passing the documentation string.
	 */
	public RenamePackageFunction() {
		super("Replaces the name and nicknames of package.");
	}

	/**
	 * {@inheritDoc}
	 * Creates the {@link RequiredParameter} list with the {@link PackageStruct} package-designator to be renamed and
	 * the package-designator new package name value.
	 *
	 * @return the list of {@link RequiredParameter} objects
	 */
	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		final List<RequiredParameter> requiredParameters = new ArrayList<>(2);
		final RequiredParameter packageParam
				= RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "PACKAGE").build();
		requiredParameters.add(packageParam);
		final RequiredParameter newNameParam
				= RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "NEW-NAME").build();
		requiredParameters.add(newNameParam);
		return requiredParameters;
	}

	/**
	 * {@inheritDoc}
	 * Creates the single {@link OptionalParameter} list of package nicknames for this function.
	 *
	 * @return a list of a single {@link OptionalParameter} list of package nicknames
	 */
	@Override
	protected List<OptionalParameter> getOptionalBindings() {
		return OptionalParameter.builder(GlobalPackageStruct.COMMON_LISP, "NEW-NICKNAMES")
		                        .suppliedPBinding()
		                        .buildList();
	}

	/**
	 * {@inheritDoc}
	 * Application method for the package function that renames the provided {@link PackageStruct} package-designator,
	 * optionally giving it the provided nicknames.
	 *
	 * @param lispStructs
	 * 		the function parameters
	 *
	 * @return the renamed {@link PackageStruct}
	 */
	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final PackageStruct aPackage = validator.validatePackageDesignator(lispStructs[0], functionName());
		final String newName = validator.validatePackageDesignatorAsString(lispStructs[1], functionName(), "New Name");

		if (lispStructs.length > 2) {
			final LispStruct lispStruct = lispStructs[2];
			validator.validateTypes(lispStruct, functionName(), "New Nicknames", ListType.INSTANCE);

			final List<LispStruct> newNicknamesList = ((ListStruct) lispStruct).getAsJavaList();

			final List<String> newNicknames
					= newNicknamesList.stream()
					                  .map(newNickname -> validator.validateStringDesignator(newNickname, functionName(), "New Nickname"))
					                  .collect(Collectors.toList());
			aPackage.renamePackage(newName, newNicknames);
		} else {
			aPackage.renamePackage(newName);
		}

		return aPackage;
	}

	/**
	 * {@inheritDoc}
	 * Returns the function name {@code rename-package} as a string.
	 *
	 * @return the function name {@code rename-package} as a string
	 */
	@Override
	protected String functionName() {
		return "RENAME-PACKAGE";
	}
}
