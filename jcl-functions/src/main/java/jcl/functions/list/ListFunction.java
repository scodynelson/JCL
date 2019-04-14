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

public final class ListFunction extends BuiltInFunctionStructImpl {

	public ListFunction() {
		super("Returns a list containing the supplied objects.",
		      CommonLispSymbols.LIST.getName(),
		      Parameters.forFunction(CommonLispSymbols.LIST.getName())
		                .restParameter()
		);
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.LIST;
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final List<LispStruct> objects = arguments.getRestArgument();
		return ListStruct.toLispList(objects);
	}
}
