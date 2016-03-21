/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages.functions;

import java.util.ArrayList;
import java.util.List;

import jcl.LispStruct;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.lists.ListStruct;
import jcl.packages.PackageStruct;
import jcl.packages.PackageVariables;
import jcl.symbols.TStruct;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code shadow}.
 */
@Component
public final class ShadowFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "SHADOW";
	private static final String SYMBOL_NAMES_ARGUMENT = "SYMBOL-NAMES";
	private static final String PACKAGE_ARGUMENT = "PACKAGE";

	/**
	 * The {@link TypeValidator} for validating the function parameter value types.
	 */
	@Autowired
	protected TypeValidator validator;

	/**
	 * Public constructor passing the documentation string.
	 */
	public ShadowFunction() {
		super("Assures that symbols with names given by symbol-names are present in the package.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(SYMBOL_NAMES_ARGUMENT)
		                .optionalParameter(PACKAGE_ARGUMENT).withInitialValue(PackageVariables.PACKAGE.getVariableValue())
		);
	}

	/**
	 * {@inheritDoc}
	 * Application method for {@code shadow} package function that shadows the provided symbol name parameters.
	 *
	 * @param lispStructs
	 * 		the function parameters
	 *
	 * @return {@link TStruct#INSTANCE}
	 */
	@Override
	public LispStruct apply(final Arguments arguments) {
		final LispStruct lispStruct = arguments.getRequiredArgument(SYMBOL_NAMES_ARGUMENT);
		final PackageStruct aPackage = arguments.getOptionalArgument(PACKAGE_ARGUMENT, PackageStruct.class);

		final String[] symbolNameArray;
		if (lispStruct instanceof ListStruct) {
			final ListStruct symbolNames = (ListStruct) lispStruct;
			final List<String> realSymbolNames = new ArrayList<>();
			for (final LispStruct theSymbolName : symbolNames) {
				final String realSymbolName = validator.validateStringDesignator(theSymbolName, functionName, "Symbol Name");
				realSymbolNames.add(realSymbolName);
			}
			symbolNameArray = realSymbolNames.toArray(new String[realSymbolNames.size()]);
		} else {
			symbolNameArray = new String[1];
			symbolNameArray[0] = validator.validateStringDesignator(lispStruct, functionName, "Symbol Name");
		}

		aPackage.shadow(symbolNameArray);

		return TStruct.INSTANCE;
	}
}
