/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions;

import jcl.lang.BooleanStruct;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.statics.CommonLispSymbols;

public final class EqualFunction extends BuiltInFunctionStructImpl {

	private static final String OBJECT1_ARGUMENT = "OBJECT-1";
	private static final String OBJECT2_ARGUMENT = "OBJECT-2";

	public EqualFunction() {
		super("Returns true if x and y are structurally similar (isomorphic) objects.",
		      CommonLispSymbols.EQUAL.getName(),
		      Parameters.forFunction(CommonLispSymbols.EQUAL.getName())
		                .requiredParameter(OBJECT1_ARGUMENT)
		                .requiredParameter(OBJECT2_ARGUMENT)
		);
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.EQUAL;
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final LispStruct object1 = arguments.getRequiredArgument(OBJECT1_ARGUMENT);
		final LispStruct object2 = arguments.getRequiredArgument(OBJECT2_ARGUMENT);
		return BooleanStruct.toLispBoolean(object1.equal(object2));
	}
}
