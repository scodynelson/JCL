/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.system;

import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.statics.CommonLispSymbols;

public final class TotalMemory extends BuiltInFunctionStructImpl {

	public TotalMemory() {
		super("Returns the current total runtime memory usage.",
		      CommonLispSymbols.TOTAL_MEMORY.getName(),
		      Parameters.forFunction(CommonLispSymbols.TOTAL_MEMORY.getName())
		);
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.TOTAL_MEMORY;
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final long totalMemory = Runtime.getRuntime().totalMemory();
		return IntegerStruct.toLispInteger(totalMemory);
	}
}
