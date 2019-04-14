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

public final class MaxMemory extends BuiltInFunctionStructImpl {

	public MaxMemory() {
		super("Returns the current max runtime memory usage.",
		      CommonLispSymbols.MAX_MEMORY.getName(),
		      Parameters.forFunction(CommonLispSymbols.MAX_MEMORY.getName())
		);
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.MAX_MEMORY;
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final long maxMemory = Runtime.getRuntime().maxMemory();
		return IntegerStruct.toLispInteger(maxMemory);
	}
}
