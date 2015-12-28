/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import java.io.Serializable;
import java.util.Arrays;
import java.util.stream.Stream;

import jcl.LispStruct;
import jcl.LispType;
import jcl.arrays.StringStruct;
import jcl.characters.CharacterStruct;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.FunctionStruct;
import jcl.packages.PackageStruct;
import jcl.printer.Printer;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class TypeValidator implements Serializable {

	private static final long serialVersionUID = -9023815286880276310L;

	@Autowired
	private Printer printer;

	@SuppressWarnings({"unchecked", "SuppressionAnnotation"})
	public <T> T validateType(final LispStruct object, final String analyzerName, final String objectName,
	                          final LispType type, final Class<T> classType) {
		validateTypes(object, analyzerName, objectName, type);
		return (T) object;
	}

	public void validateTypes(final LispStruct object, final String functionName, final String objectName,
	                          final LispType... types) {

		final LispType type = object.getType();
		final boolean noneMatch = Stream.of(types)
		                                .noneMatch(e -> e.equals(type));

		if (noneMatch) {
			final String printedObject = printer.print(object);
			throw new TypeErrorException(functionName + ": " + objectName + " must be one of the following types: " + Arrays.toString(types) + ". Got: " + printedObject);
		}
	}

	public PackageStruct validatePackageDesignator(final LispStruct packageDesignator, final String functionName) {
		validateTypes(packageDesignator, functionName, "Package", StringType.INSTANCE, SymbolType.INSTANCE, CharacterType.INSTANCE, PackageType.INSTANCE);

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

	public String validateStringDesignator(final LispStruct stringDesignator, final String functionName, final String parameterName) {
		validateTypes(stringDesignator, functionName, parameterName, StringType.INSTANCE, SymbolType.INSTANCE, CharacterType.INSTANCE);

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

	public String validatePackageDesignatorAsString(final LispStruct packageDesignator, final String functionName, final String parameterName) {
		validateTypes(packageDesignator, functionName, parameterName, StringType.INSTANCE, SymbolType.INSTANCE, CharacterType.INSTANCE, PackageType.INSTANCE);

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

	public FunctionStruct validateFunctionDesignator(final LispStruct functionDesignator, final String functionName, final String parameterName) {
		validateTypes(functionDesignator, functionName, parameterName, FunctionType.INSTANCE, SymbolType.INSTANCE);

		if (functionDesignator instanceof FunctionStruct) {
			return (FunctionStruct) functionDesignator;
		} else if (functionDesignator instanceof SymbolStruct) {
			return ((SymbolStruct) functionDesignator).getFunction();
		} else {
			throw new TypeErrorException("UNCAUGHT TYPE ERROR.");
		}
	}
}
