/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.symbol;

import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.LispStruct;
import jcl.lang.PackageStruct;
import jcl.lang.StringStruct;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.statics.PackageVariables;
import org.springframework.stereotype.Component;

@Component
public final class GentempFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "GENTEMP";
	private static final String PREFIX_ARGUMENT = "PREFIX";
	private static final String PACKAGE_ARGUMENT = "PACKAGE";

	private int gentempCounter;

	public GentempFunction() {
		super("Creates and returns a fresh symbol, interned in the indicated package.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .optionalParameter(PREFIX_ARGUMENT).withInitialValue(LispStructFactory.toString("T"))
		                .optionalParameter(PACKAGE_ARGUMENT).withInitialValue(PackageVariables.PACKAGE.getVariableValue())
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		String prefix = arguments.getOptionalArgument(PREFIX_ARGUMENT, StringStruct.class).getAsJavaString();
		PackageStruct aPackage = arguments.getOptionalArgument(PACKAGE_ARGUMENT).asPackage().get();

		String symbolName = prefix + gentempCounter++;
		while (aPackage.findSymbol(symbolName) != null) {
			symbolName = prefix + gentempCounter++;
		}
		return aPackage.intern(symbolName).getSymbol();
	}
}
