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

public final class EqlFunction extends BuiltInFunctionStructImpl {

	private static final String OBJECT1_ARGUMENT = "OBJECT-1";
	private static final String OBJECT2_ARGUMENT = "OBJECT-2";

	public EqlFunction() {
		super("Returns true if its arguments are the same, identical object; otherwise, returns false.",
		      CommonLispSymbols.EQL.getName(),
		      Parameters.forFunction(CommonLispSymbols.EQL.getName())
		                .requiredParameter(OBJECT1_ARGUMENT)
		                .requiredParameter(OBJECT2_ARGUMENT)
		);
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.EQL;
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final LispStruct object1 = arguments.getRequiredArgument(OBJECT1_ARGUMENT);
		final LispStruct object2 = arguments.getRequiredArgument(OBJECT2_ARGUMENT);
		return BooleanStruct.toLispBoolean(object1.eql(object2));
	}
}
