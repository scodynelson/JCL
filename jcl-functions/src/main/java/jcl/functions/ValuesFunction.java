/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions;

import java.util.List;

import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.ValuesStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.statics.CommonLispSymbols;

public final class ValuesFunction extends BuiltInFunctionStructImpl {

	public ValuesFunction() {
		super("Returns the objects as multiple values.",
		      CommonLispSymbols.VALUES.getName(),
		      Parameters.forFunction(CommonLispSymbols.VALUES.getName())
		                .restParameter()
		);
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.VALUES;
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final List<LispStruct> valuesList = arguments.getRestArgument();
		return ValuesStruct.valueOf(valuesList);
	}
}
