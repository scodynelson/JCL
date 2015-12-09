/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages.functions;

import java.util.ArrayList;
import java.util.List;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.characters.CharacterStruct;
import jcl.compiler.environment.binding.lambdalist.OptionalParameter;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.symbols.SymbolStruct;
import jcl.types.CharacterType;
import jcl.types.ListType;
import jcl.types.PackageType;
import jcl.types.StringType;
import jcl.types.SymbolType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class RenamePackageFunction extends AbstractCommonLispFunctionStruct {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -5908168151723138190L;

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
	public RenamePackageFunction() {
		super("Replaces the name and nicknames of package.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		final List<RequiredParameter> requiredParameters = new ArrayList<>(2);
		final RequiredParameter packageParam
				= new RequiredParameter.Builder(GlobalPackageStruct.COMMON_LISP, "PACKAGE").build();
		requiredParameters.add(packageParam);
		final RequiredParameter newNameParam
				= new RequiredParameter.Builder(GlobalPackageStruct.COMMON_LISP, "NEW-NAME").build();
		requiredParameters.add(newNameParam);
		return requiredParameters;
	}

	@Override
	protected List<OptionalParameter> getOptionalBindings() {
		return new OptionalParameter.Builder(GlobalPackageStruct.COMMON_LISP, "NEW-NICKNAMES")
				.suppliedPBinding()
				.buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final PackageStruct aPackage = findPackageFunction.findPackage(lispStructs[0]);

		final LispStruct newName = lispStructs[1];
		final String realNewName = getStringFromPackageDesignator(newName, "New Name");

		final List<String> newNicknames = new ArrayList<>();
		if (lispStructs.length > 2) {
			final LispStruct lispStruct = lispStructs[2];
			validator.validateTypes(lispStruct, functionName(), "New Nicknames", ListType.INSTANCE);

			final ListStruct newNicknamesListStruct = (ListStruct) lispStruct;
			final List<LispStruct> newNicknamesList = newNicknamesListStruct.getAsJavaList();
			for (final LispStruct newNickname : newNicknamesList) {
				final String newNicknameString = getStringFromStringDesignator(newNickname, "New Nickname");
				newNicknames.add(newNicknameString);
			}
		}

		aPackage.renamePackage(realNewName, newNicknames);
		return aPackage;
	}

	private String getStringFromPackageDesignator(final LispStruct packageDesignator, final String parameterName) {
		validator.validateTypes(packageDesignator, functionName(), parameterName, StringType.INSTANCE, SymbolType.INSTANCE, CharacterType.INSTANCE, PackageType.INSTANCE);

		if (packageDesignator instanceof StringStruct) {
			return ((StringStruct) packageDesignator).getAsJavaString();
		} else if (packageDesignator instanceof SymbolStruct) {
			return ((SymbolStruct) packageDesignator).getName();
		} else if (packageDesignator instanceof CharacterStruct) {
			return ((CharacterStruct) packageDesignator).getCharacter().toString();
		} else if (packageDesignator instanceof PackageStruct) {
			return ((PackageStruct) packageDesignator).getName();
		} else {
			throw new TypeErrorException("UNCAUGHT TYPE ERROR.");
		}
	}

	private String getStringFromStringDesignator(final LispStruct stringDesignator, final String parameterName) {
		validator.validateTypes(stringDesignator, functionName(), "String-Designator", StringType.INSTANCE, SymbolType.INSTANCE, CharacterType.INSTANCE);

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
		return "RENAME-PACKAGE";
	}
}
