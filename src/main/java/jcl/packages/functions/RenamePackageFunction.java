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
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.symbols.SymbolStruct;
import jcl.types.CharacterType;
import jcl.types.ListType;
import jcl.types.PackageType;
import jcl.types.StringType;
import jcl.types.SymbolType;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code rename-package}.
 */
@Component
public final class RenamePackageFunction extends AbstractPackageFunction {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -5908168151723138190L;

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

		final PackageStruct aPackage = findPackage(lispStructs[0]);

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
