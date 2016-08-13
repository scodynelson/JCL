/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.lisppackage;

import java.util.ArrayList;
import java.util.List;

import jcl.lang.LispStruct;
import jcl.lang.PackageStruct;
import jcl.lang.statics.PackageVariables;
import jcl.lang.TStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStructBase;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.ListStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code shadow}.
 */
@Component
public final class ShadowFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "SHADOW";
	private static final String SYMBOL_NAMES_ARGUMENT = "SYMBOL-NAMES";
	private static final String PACKAGE_ARGUMENT = "PACKAGE";

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
				final String realSymbolName = theSymbolName.asString().get().getAsJavaString();
				realSymbolNames.add(realSymbolName);
			}
			symbolNameArray = realSymbolNames.toArray(new String[realSymbolNames.size()]);
		} else {
			symbolNameArray = new String[1];
			symbolNameArray[0] = lispStruct.asString().get().getAsJavaString();
		}

		aPackage.shadow(symbolNameArray);

		return TStruct.INSTANCE;
	}
}
