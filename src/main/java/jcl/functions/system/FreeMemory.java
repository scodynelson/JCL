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

public final class FreeMemory extends BuiltInFunctionStructImpl {

	public FreeMemory() {
		super("Returns the current free runtime memory usage.",
		      CommonLispSymbols.FREE_MEMORY.getName(),
		      Parameters.forFunction(CommonLispSymbols.FREE_MEMORY.getName())
		);
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.FREE_MEMORY;
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final long freeMemory = Runtime.getRuntime().freeMemory();
		return IntegerStruct.toLispInteger(freeMemory);
	}
}
