/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages.functions;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.KeyParameter;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.symbols.KeywordStruct;
import jcl.symbols.NILStruct;
import jcl.system.CommonLispSymbols;
import jcl.types.ListType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code make-package}.
 */
@Component
public final class MakePackageFunction extends AbstractCommonLispFunctionStruct {

	/**
	 * The {@link TypeValidator} for validating the function parameter value types.
	 */
	@Autowired
	private TypeValidator validator;

	/**
	 * Public constructor passing the documentation string.
	 */
	public MakePackageFunction() {
		super("Creates a new package with the name package-name.");
	}

	/**
	 * {@inheritDoc}
	 * Creates the single {@link RequiredParameter} string-designator object for this function.
	 *
	 * @return a list of a single {@link RequiredParameter} string-designator object
	 */
	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "PACKAGE-NAME").buildList();
	}

	/**
	 * {@inheritDoc}
	 * Create the list of {@link KeyParameter}s for the new {@link PackageStruct} nicknames and packages for it to use.
	 *
	 * @return a list of {@link KeyParameter}s
	 */
	@Override
	protected List<KeyParameter> getKeyBindings() {
		final List<KeyParameter> keyParameters = new ArrayList<>(2);
		final KeyParameter nicknamesParam
				= KeyParameter.builder(GlobalPackageStruct.COMMON_LISP, "NICKNAMES")
				              .suppliedPBinding()
				              .build();
		keyParameters.add(nicknamesParam);
		final KeyParameter useParam
				= KeyParameter.builder(GlobalPackageStruct.COMMON_LISP, "USE")
				              .initForm(ListStruct.buildProperList(GlobalPackageStruct.COMMON_LISP))
				              .suppliedPBinding()
				              .build();
		keyParameters.add(useParam);
		return keyParameters;
	}

	/**
	 * {@inheritDoc}
	 * Application method for the package function that creates a new {@link PackageStruct} object with the provided
	 * string-designator package name and the optional nicknames list and packages to use list.
	 *
	 * @param lispStructs
	 * 		the function parameters
	 *
	 * @return the newly created {@link PackageStruct} object
	 */
	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		final String packageName = validator.validateStringDesignator(lispStruct, functionName(), "Package Name");

		if (PackageStruct.findPackage(packageName) != null) {
			throw new ProgramErrorException("Package name " + packageName + " is already in use.");
		}

		final Map<KeywordStruct, LispStruct> keywords
				= getKeywords(lispStructs, 1, CommonLispSymbols.NICKNAMES_KEYWORD, CommonLispSymbols.USE_KEYWORD);

		final LispStruct nicknames
				= keywords.getOrDefault(CommonLispSymbols.NICKNAMES_KEYWORD, NILStruct.INSTANCE);
		validator.validateTypes(nicknames, functionName(), "Nicknames", ListType.INSTANCE);

		final LispStruct usePackages
				= keywords.getOrDefault(CommonLispSymbols.USE_KEYWORD, ListStruct.buildProperList(GlobalPackageStruct.COMMON_LISP));
		validator.validateTypes(usePackages, functionName(), "Use Packages", ListType.INSTANCE);

		final List<LispStruct> nicknamesList = ((ListStruct) nicknames).getAsJavaList();
		final List<String> realNicknames
				= nicknamesList.stream()
				               .map(e -> validator.validateStringDesignator(nicknames, functionName(), "Nickname"))
				               .collect(Collectors.toList());

		final List<LispStruct> usePackagesList = ((ListStruct) usePackages).getAsJavaList();
		final Set<PackageStruct> realUsePackages
				= usePackagesList.stream()
				                 .map(e -> validator.validatePackageDesignator(e, functionName()))
				                 .collect(Collectors.toSet());

		return new PackageStruct(packageName, realNicknames, realUsePackages);
	}

	/**
	 * {@inheritDoc}
	 * Returns the function name {@code make-package} as a string.
	 *
	 * @return the function name {@code make-package} as a string
	 */
	@Override
	protected String functionName() {
		return "MAKE-PACKAGE";
	}
}
