/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.symbol;

import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.StringStruct;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.statics.SymbolVariables;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class GensymFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "GENSYM";
	private static final String PREFIX_ARGUMENT = "PREFIX";

	public GensymFunction() {
		super("Creates and returns a fresh, uninterned symbol.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .optionalParameter(PREFIX_ARGUMENT).withInitialValue(null)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {

		String gensymPrefix = "G";
		IntegerStruct gensymPostfix = SymbolVariables.GENSYM_COUNTER.getVariableValue();
		final LispStruct defaulting = arguments.getOptionalArgument(PREFIX_ARGUMENT);
//		validator.validateTypes(defaulting, functionName(), "Prefix", StringType.INSTANCE, IntegerType.INSTANCE);
		if (defaulting != null) {

			if (defaulting instanceof StringStruct) {
				gensymPrefix = ((StringStruct) defaulting).getAsJavaString();
				SymbolVariables.GENSYM_COUNTER.setValue(gensymPostfix.add(IntegerStruct.ONE));
			} else if (defaulting instanceof IntegerStruct) {
				gensymPostfix = (IntegerStruct) defaulting;
			} else {
				throw new TypeErrorException("UNCAUGHT TYPE ERROR.");
			}
		} else {
			SymbolVariables.GENSYM_COUNTER.setValue(gensymPostfix.add(IntegerStruct.ONE));
		}

		final String symbolName = gensymPrefix + gensymPostfix.bigIntegerValue();
		return LispStructFactory.toSymbol(symbolName);
	}
}
