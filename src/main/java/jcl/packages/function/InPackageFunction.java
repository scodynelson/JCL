/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages.function;

import java.util.Collections;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.characters.CharacterStruct;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.real.environment.binding.lambdalist.RequiredParameter;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.FunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.packages.PackageVariables;
import jcl.printer.Printer;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class InPackageFunction extends FunctionStruct {

	public static final SymbolStruct<?> IN_PACKAGE_FUNCTION = GlobalPackageStruct.COMMON_LISP.intern("IN-PACKAGE").getSymbol();

	private static final long serialVersionUID = 5831829564256951336L;

	@Autowired
	private Printer printer;

	private InPackageFunction() {
		super("Causes the the package named by name to become the current package.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		IN_PACKAGE_FUNCTION.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(IN_PACKAGE_FUNCTION);
	}

	private static OrdinaryLambdaList getInitLambdaListBindings() {

		final SymbolStruct<?> nameArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("NAME").getSymbol();
		final RequiredParameter nameArgRequiredBinding = new RequiredParameter(nameArgSymbol);
		final List<RequiredParameter> requiredBindings = Collections.singletonList(nameArgRequiredBinding);

		return new OrdinaryLambdaList.Builder().requiredBindings(requiredBindings)
		                                       .build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

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
}
