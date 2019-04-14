/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.list;

import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.statics.CommonLispSymbols;

public final class CdrFunction extends BuiltInFunctionStructImpl {

	private static final String LIST_ARGUMENT = "LIST";

	public CdrFunction() {
		super("Gets the cdr of the provided list.",
		      CommonLispSymbols.CDR.getName(),
		      Parameters.forFunction(CommonLispSymbols.CDR.getName())
		                .requiredParameter(LIST_ARGUMENT)
		);
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.CDR;
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final ListStruct list = arguments.getRequiredArgument(LIST_ARGUMENT, ListStruct.class);
		return list.cdr();
	}
}
