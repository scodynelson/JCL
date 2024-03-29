/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.symbol;

import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.StringStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.statics.CommonLispSymbols;

public final class GensymFunction extends BuiltInFunctionStructImpl {

	private static final String PREFIX_ARGUMENT = "PREFIX";

	public GensymFunction() {
		super("Creates and returns a fresh, uninterned symbol.",
		      CommonLispSymbols.GENSYM.getName(),
		      Parameters.forFunction(CommonLispSymbols.GENSYM.getName())
		                .optionalParameter(PREFIX_ARGUMENT).withInitialValue(null)
		);
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.GENSYM;
	}

	@Override
	public LispStruct apply(final Arguments arguments) {

		String gensymPrefix = "G";
		IntegerStruct gensymPostfix = CommonLispSymbols.GENSYM_COUNTER_VAR.getVariableValue();
		final LispStruct defaulting = arguments.getOptionalArgument(PREFIX_ARGUMENT);
//		validator.validateTypes(defaulting, functionName(), "Prefix", StringType.INSTANCE, IntegerType.INSTANCE);
		if (defaulting != null) {

			if (defaulting instanceof StringStruct) {
				gensymPrefix = ((StringStruct) defaulting).toJavaString();
				CommonLispSymbols.GENSYM_COUNTER_VAR.setfSymbolValue(gensymPostfix.add(IntegerStruct.ONE));
			} else if (defaulting instanceof IntegerStruct) {
				gensymPostfix = (IntegerStruct) defaulting;
			} else {
				throw new TypeErrorException("UNCAUGHT TYPE ERROR.");
			}
		} else {
			CommonLispSymbols.GENSYM_COUNTER_VAR.setfSymbolValue(gensymPostfix.add(IntegerStruct.ONE));
		}

		final String symbolName = gensymPrefix + gensymPostfix;
		return SymbolStruct.toLispSymbol(symbolName);
	}
}
