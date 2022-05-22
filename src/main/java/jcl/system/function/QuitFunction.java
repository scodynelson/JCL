/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.system.function;

import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.statics.CommonLispSymbols;

public final class QuitFunction extends BuiltInFunctionStructImpl {

	public QuitFunction() {
		super("Quits the JCL Application.",
		      CommonLispSymbols.QUIT.getName(),
		      Parameters.forFunction(CommonLispSymbols.QUIT.getName())
		);
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.QUIT;
	}

	@SuppressWarnings("all")
	@Override
	public LispStruct apply(final Arguments arguments) {
		System.exit(0);
		return null;
	}
}
