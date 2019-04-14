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

public final class AppendFunction extends BuiltInFunctionStructImpl {

	public AppendFunction() {
		super("Returns a new list that is the concatenation of the copies.",
		      CommonLispSymbols.APPEND.getName(),
		      Parameters.forFunction(CommonLispSymbols.APPEND.getName())
		                .restParameter()
		);
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.APPEND;
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final List<LispStruct> restArgument = arguments.getRestArgument();
		return ListStruct.append(restArgument);
	}
}
