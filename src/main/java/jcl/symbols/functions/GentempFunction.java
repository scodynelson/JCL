/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.symbols.functions;

import java.util.Arrays;
import java.util.List;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.compiler.environment.binding.lambdalist.OptionalParameter;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.packages.PackageVariables;
import jcl.types.StringType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class GentempFunction extends AbstractCommonLispFunctionStruct {

	/**
	 * The {@link TypeValidator} for validating the function parameter value types.
	 */
	@Autowired
	private TypeValidator validator;

	private int gentempCounter;

	public GentempFunction() {
		super("Creates and returns a fresh symbol, interned in the indicated package.");
	}

	@Override
	protected List<OptionalParameter> getOptionalBindings() {
		final OptionalParameter prefix =
				OptionalParameter.builder(GlobalPackageStruct.COMMON_LISP, "PREFIX")
				                 .initForm(new StringStruct("T"))
				                 .suppliedPBinding()
				                 .build();
		final OptionalParameter aPackage =
				OptionalParameter.builder(GlobalPackageStruct.COMMON_LISP, "PACKAGE")
				                 .initForm(PackageVariables.PACKAGE.getVariableValue())
				                 .suppliedPBinding()
				                 .build();
		return Arrays.asList(prefix, aPackage);
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		String prefix = "T";
		PackageStruct aPackage = PackageVariables.PACKAGE.getVariableValue();

		if (lispStructs.length > 0) {
			final StringStruct prefixVal
					= validator.validateType(lispStructs[0], functionName(), "Prefix", StringType.INSTANCE, StringStruct.class);
			prefix = prefixVal.getAsJavaString();
		}
		if (lispStructs.length > 1) {
			aPackage = lispStructs[1].asPackage().get();
		}

		String symbolName = prefix + gentempCounter++;
		while (aPackage.findSymbol(symbolName) != null) {
			symbolName = prefix + gentempCounter++;
		}
		return aPackage.intern(symbolName).getSymbol();
	}

	@Override
	protected String functionName() {
		return "GENTEMP";
	}
}
