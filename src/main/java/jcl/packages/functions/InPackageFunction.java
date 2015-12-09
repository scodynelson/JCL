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
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.packages.PackageVariables;
import jcl.printer.Printer;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class InPackageFunction extends AbstractCommonLispFunctionStruct {

	private static final long serialVersionUID = 5831829564256951336L;

	@Autowired
	private Printer printer;

	private InPackageFunction() {
		super("Causes the the package named by name to become the current package.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return new RequiredParameter.Builder(GlobalPackageStruct.COMMON_LISP, "NAME").buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct name = lispStructs[0];

		final String newCurrentPackageName;
		if (name instanceof StringStruct) {
			newCurrentPackageName = ((StringStruct) name).getAsJavaString();
		} else if (name instanceof CharacterStruct) {
			final char character = ((CharacterStruct) name).getCharacter();
			newCurrentPackageName = String.valueOf(character);
		} else if (name instanceof SymbolStruct) {
			newCurrentPackageName = ((SymbolStruct) name).getName();
		} else {
			final String printedObject = printer.print(name);
			throw new TypeErrorException("Name is not a valid string designator: " + printedObject);
		}

		final PackageStruct newCurrentPackage = PackageStruct.findPackage(newCurrentPackageName);
		PackageVariables.PACKAGE.setValue(newCurrentPackage);

		return newCurrentPackage;
	}

	@Override
	protected String functionName() {
		return "IN-PACKAGE";
	}
}
