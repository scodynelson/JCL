/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages.functions;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.characters.CharacterStruct;
import jcl.compiler.environment.binding.lambdalist.KeyParameter;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.symbols.SymbolStruct;
import jcl.system.CommonLispSymbols;
import jcl.types.CharacterType;
import jcl.types.ListType;
import jcl.types.StringType;
import jcl.types.SymbolType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class MakePackageFunction extends AbstractCommonLispFunctionStruct {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -1982336595153324433L;

	/**
	 * The {@link TypeValidator} for validating the function parameter value types.
	 */
	@Autowired
	private TypeValidator validator;

	@Autowired
	private FindPackageFunction findPackageFunction;

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
			final PackageStruct aPackage = findPackageFunction.findPackage(usePackage);
			realUsePackages.add(aPackage);
		}

		return new PackageStruct(packageName, realNicknames, realUsePackages);
	}

	private String getStringFromStringDesignator(final LispStruct stringDesignator, final String parameterName) {
		validator.validateTypes(stringDesignator, functionName(), parameterName, StringType.INSTANCE, SymbolType.INSTANCE, CharacterType.INSTANCE);

		if (stringDesignator instanceof StringStruct) {
			return ((StringStruct) stringDesignator).getAsJavaString();
		} else if (stringDesignator instanceof SymbolStruct) {
			return ((SymbolStruct) stringDesignator).getName();
		} else if (stringDesignator instanceof CharacterStruct) {
			return ((CharacterStruct) stringDesignator).getCharacter().toString();
		} else {
			throw new TypeErrorException("UNCAUGHT TYPE ERROR.");
		}
	}

	@Override
	protected String functionName() {
		return "MAKE-PACKAGE";
	}
}
