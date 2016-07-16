/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.symbols.functions;

import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.SymbolVariables;
import jcl.lang.array.StringStruct;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.number.IntegerStruct;
import org.springframework.stereotype.Component;

@Component
public final class GensymFunction extends CommonLispBuiltInFunctionStruct {

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
		return new SymbolStruct(symbolName);
	}
}
