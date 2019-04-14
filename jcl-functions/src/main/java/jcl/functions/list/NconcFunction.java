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

public final class NconcFunction extends BuiltInFunctionStructImpl {

	public NconcFunction() {
		super("Returns a list that is the concatenation of lists.",
		      CommonLispSymbols.NCONC.getName(),
		      Parameters.forFunction(CommonLispSymbols.NCONC.getName())
		                .restParameter()
		);
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.NCONC;
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final List<LispStruct> restArgument = arguments.getRestArgument();
		return ListStruct.nConc(restArgument);
	}
}
