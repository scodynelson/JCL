/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages.functions;

import java.util.List;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.characters.CharacterStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.lists.NullStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.symbols.SymbolStruct;
import jcl.types.CharacterType;
import jcl.types.PackageType;
import jcl.types.StringType;
import jcl.types.SymbolType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class FindPackageFunction extends AbstractCommonLispFunctionStruct {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -3051378531665513323L;

	/**
	 * The {@link TypeValidator} for validating the function parameter value types.
	 */
	@Autowired
	private TypeValidator validator;

	/**
	 * Public constructor passing the documentation string.
	 */
	public FindPackageFunction() {
		super("Locates and returns the package whose name or nickname is name.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return new RequiredParameter.Builder(GlobalPackageStruct.COMMON_LISP, "NAME").buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct packageDesignator = lispStructs[0];
		final PackageStruct aPackage = findPackage(packageDesignator);
		return (aPackage == null) ? NullStruct.INSTANCE : aPackage;
	}

	public PackageStruct findPackage(final LispStruct packageDesignator) {
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

	@Override
	protected String functionName() {
		return "FIND-PACKAGE";
	}
}
