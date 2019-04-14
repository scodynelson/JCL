/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.list;

import java.util.List;

import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.statics.CommonLispSymbols;

public final class ListStarFunction extends BuiltInFunctionStructImpl {

	private static final String ARG_ARGUMENT = "SIZE";

	public ListStarFunction() {
		super("Returns a list containing the supplied objects where the last argument becomes the cdr of the last cons constructed.",
		      CommonLispSymbols.LIST_STAR.getName(),
		      Parameters.forFunction(CommonLispSymbols.LIST_STAR.getName())
		                .requiredParameter(ARG_ARGUMENT)
		                .restParameter()
		);
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.LIST_STAR;
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final LispStruct arg = arguments.getRequiredArgument(ARG_ARGUMENT);
		final List<LispStruct> objects = arguments.getRestArgument();
		return ListStruct.toLispDottedList(arg, objects);
	}
}
