/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.list;

import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.ConsStruct;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.statics.CommonLispSymbols;

public final class ConsFunction extends BuiltInFunctionStructImpl {

	private static final String OBJECT_1_ARGUMENT = "OBJECT1";
	private static final String OBJECT_2_ARGUMENT = "OBJECT2";

	public ConsFunction() {
		super("Creates a fresh cons, the car of which is object-1 and the cdr of which is object-2.",
		      CommonLispSymbols.CONS.getName(),
		      Parameters.forFunction(CommonLispSymbols.CONS.getName())
		                .requiredParameter(OBJECT_1_ARGUMENT)
		                .requiredParameter(OBJECT_2_ARGUMENT)
		);
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.CONS;
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final LispStruct object1 = arguments.getRequiredArgument(OBJECT_1_ARGUMENT);
		final LispStruct object2 = arguments.getRequiredArgument(OBJECT_2_ARGUMENT);
		return ConsStruct.toLispCons(object1, object2);
	}
}
