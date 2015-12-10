/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages.functions;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.KeyParameter;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.system.CommonLispSymbols;
import jcl.types.ListType;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code make-package}.
 */
@Component
public final class MakePackageFunction extends AbstractPackageFunction {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -1982336595153324433L;

	/**
	 * Public constructor passing the documentation string.
	 */
	public MakePackageFunction() {
		super("Creates a new package with the name package-name.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return new RequiredParameter.Builder(GlobalPackageStruct.COMMON_LISP, "PACKAGE-NAME").buildList();
	}

	@Override
	protected List<KeyParameter> getKeyBindings() {
		final List<KeyParameter> keyParameters = new ArrayList<>(2);
		final KeyParameter nicknamesParam
				= new KeyParameter.Builder(GlobalPackageStruct.COMMON_LISP, "NICKNAMES")
				.suppliedPBinding()
				.build();
		keyParameters.add(nicknamesParam);
		final KeyParameter useParam
				= new KeyParameter.Builder(GlobalPackageStruct.COMMON_LISP, "USE")
				.initForm(ListStruct.buildProperList(GlobalPackageStruct.COMMON_LISP))
				.suppliedPBinding()
				.build();
		keyParameters.add(useParam);
		return keyParameters;
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		final String packageName = getStringFromStringDesignator(lispStruct, "Package Name");

		if (PackageStruct.findPackage(packageName) != null) {
			throw new ProgramErrorException("Package name " + packageName + " is already in use.");
		}

		ListStruct nicknames = NullStruct.INSTANCE;
		ListStruct usePackages = ListStruct.buildProperList(GlobalPackageStruct.COMMON_LISP);

		final int length = lispStructs.length;
		if (length >= 3) {
			// 1 keyword
			final LispStruct firstKeyword = lispStructs[1];
			final LispStruct keyValue = lispStructs[2];
			if (CommonLispSymbols.NICKNAMES_KEYWORD.equals(firstKeyword)) {
				validator.validateTypes(keyValue, functionName(), "Nicknames", ListType.INSTANCE);
				nicknames = (ListStruct) keyValue;
			} else if (CommonLispSymbols.USE_KEYWORD.equals(firstKeyword)) {
				validator.validateTypes(keyValue, functionName(), "Use Packages", ListType.INSTANCE);
				usePackages = (ListStruct) keyValue;
			}
		}
		if (length >= 5) {
			// 2 keywords
			final LispStruct secondKeyword = lispStructs[3];
			final LispStruct keyValue = lispStructs[4];
			if (CommonLispSymbols.NICKNAMES_KEYWORD.equals(secondKeyword)) {
				validator.validateTypes(keyValue, functionName(), "Nicknames", ListType.INSTANCE);
				nicknames = (ListStruct) keyValue;
			} else if (CommonLispSymbols.USE_KEYWORD.equals(secondKeyword)) {
				validator.validateTypes(keyValue, functionName(), "Use Packages", ListType.INSTANCE);
				usePackages = (ListStruct) keyValue;
			}
		}

		final List<LispStruct> nicknamesList = nicknames.getAsJavaList();
		final List<String> realNicknames = new ArrayList<>(nicknamesList.size());
		for (final LispStruct nickname : nicknamesList) {
			final String nicknameString = getStringFromStringDesignator(nickname, "Nickname");
			realNicknames.add(nicknameString);
		}

		final List<LispStruct> usePackagesList = usePackages.getAsJavaList();
		final Set<PackageStruct> realUsePackages = new HashSet<>(usePackagesList.size());
		for (final LispStruct usePackage : usePackagesList) {
			final PackageStruct aPackage = findPackage(usePackage);
			realUsePackages.add(aPackage);
		}

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
