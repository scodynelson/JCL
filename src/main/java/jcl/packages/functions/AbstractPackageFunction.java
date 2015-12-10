/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages.functions;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.characters.CharacterStruct;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.packages.PackageStruct;
import jcl.symbols.SymbolStruct;
import jcl.types.CharacterType;
import jcl.types.PackageType;
import jcl.types.StringType;
import jcl.types.SymbolType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;

abstract class AbstractPackageFunction extends AbstractCommonLispFunctionStruct {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 2007009672631705124L;

	/**
	 * The {@link TypeValidator} for validating the function parameter value types.
	 */
	@Autowired
	protected TypeValidator validator;

	protected AbstractPackageFunction(final String documentation) {
		super(documentation);
	}

	protected PackageStruct findPackage(final LispStruct packageDesignator) {
		validator.validateTypes(packageDesignator, functionName(), "Package", StringType.INSTANCE, SymbolType.INSTANCE, CharacterType.INSTANCE, PackageType.INSTANCE);

		if (packageDesignator instanceof StringStruct) {
			final String packageName = ((StringStruct) packageDesignator).getAsJavaString();
			return PackageStruct.findPackage(packageName);
		} else if (packageDesignator instanceof SymbolStruct) {
			final String packageName = ((SymbolStruct) packageDesignator).getName();
			return PackageStruct.findPackage(packageName);
		} else if (packageDesignator instanceof CharacterStruct) {
			final String packageName = ((CharacterStruct) packageDesignator).getCharacter().toString();
			return PackageStruct.findPackage(packageName);
		} else if (packageDesignator instanceof PackageStruct) {
			return (PackageStruct) packageDesignator;
		} else {
			throw new TypeErrorException("UNCAUGHT TYPE ERROR.");
		}
	}

	protected String getStringFromStringDesignator(final LispStruct stringDesignator, final String parameterName) {
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

	protected String getStringFromPackageDesignator(final LispStruct packageDesignator, final String parameterName) {
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
}
