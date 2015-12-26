/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.symbols.functions;

import java.util.ArrayList;
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
public class GentempFunction extends AbstractCommonLispFunctionStruct {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -8752679249572943124L;

	/**
	 * The {@link TypeValidator} for validating the function parameter value types.
	 */
	@Autowired
	private TypeValidator validator;

	private int gentempCounter;

	public GentempFunction() {
		super("Creates and returns a fresh, uninterned symbol.");
	}

	@Override
	protected List<OptionalParameter> getOptionalBindings() {
		final List<OptionalParameter> optionalParameters = new ArrayList<>(2);

		final OptionalParameter prefix = new OptionalParameter.Builder(GlobalPackageStruct.COMMON_LISP, "PREFIX")
				.initForm(new StringStruct("T"))
				.suppliedPBinding()
				.build();
		optionalParameters.add(prefix);

		final OptionalParameter aPackage = new OptionalParameter.Builder(GlobalPackageStruct.COMMON_LISP, "PACKAGE")
				.initForm(PackageVariables.PACKAGE.getVariableValue())
				.suppliedPBinding()
				.build();
		optionalParameters.add(aPackage);

		return optionalParameters;
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		String prefix = "T";
		PackageStruct aPackage = PackageVariables.PACKAGE.getVariableValue();

		if (lispStructs.length > 0) {
			final LispStruct lispStruct = lispStructs[0];
			validator.validateTypes(lispStruct, functionName(), "Prefix", StringType.INSTANCE);

			prefix = ((StringStruct) lispStruct).getAsJavaString();
		}
		if (lispStructs.length > 1) {
			aPackage = validator.validatePackageDesignator(lispStructs[1], functionName());
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
