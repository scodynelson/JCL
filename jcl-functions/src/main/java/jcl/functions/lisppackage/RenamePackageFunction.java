/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.lisppackage;

import java.util.List;
import java.util.function.Supplier;
import java.util.stream.Collectors;

import jcl.lang.CharacterStruct;
import jcl.lang.LispStruct;
import jcl.lang.PackageStruct;
import jcl.lang.StringStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.list.ListStruct;
import jcl.lang.list.NILStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code rename-package}.
 */
@Component
public final class RenamePackageFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "RENAME-PACKAGE";
	private static final String PACKAGE_ARGUMENT = "PACKAGE";
	private static final String NEW_NAME_ARGUMENT = "NEW-NAME";
	private static final String NEW_NICKNAMES_ARGUMENT = "NEW-NICKNAMES";

	/**
	 * Public constructor passing the documentation string.
	 */
	public RenamePackageFunction() {
		super("Replaces the name and nicknames of package.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(PACKAGE_ARGUMENT)
		                .requiredParameter(NEW_NAME_ARGUMENT)
		                .optionalParameter(NEW_NICKNAMES_ARGUMENT).withInitialValue(NILStruct.INSTANCE)
		);
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
	public LispStruct apply(final Arguments arguments) {
		final PackageStruct aPackage = arguments.getRequiredArgument(PACKAGE_ARGUMENT).asPackage().get();

		final LispStruct packageDesignator = arguments.getRequiredArgument(NEW_NAME_ARGUMENT);
		final String newName;
		if (packageDesignator instanceof StringStruct) {
			newName = ((StringStruct) packageDesignator).getAsJavaString();
		} else if (packageDesignator instanceof SymbolStruct) {
			newName = ((SymbolStruct) packageDesignator).getName();
		} else if (packageDesignator instanceof CharacterStruct) {
			newName = ((CharacterStruct) packageDesignator).getCharacter().toString();
		} else if (packageDesignator instanceof PackageStruct) {
			newName = ((PackageStruct) packageDesignator).getName();
		} else {
			throw new TypeErrorException("UNCAUGHT TYPE ERROR.");
		}

		if (arguments.hasOptionalArgument(NEW_NICKNAMES_ARGUMENT)) {
			final ListStruct newNicknamesList = arguments.getOptionalArgument(NEW_NICKNAMES_ARGUMENT, ListStruct.class);
			final List<String> newNicknames
					= newNicknamesList.stream()
					                  .map(LispStruct::asString)
					                  .map(Supplier::get)
					                  .map(StringStruct::getAsJavaString)
					                  .collect(Collectors.toList());
			aPackage.renamePackage(newName, newNicknames);
		} else {
			aPackage.renamePackage(newName);
		}

		return aPackage;
	}
}
