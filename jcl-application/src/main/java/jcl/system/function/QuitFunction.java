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
import org.springframework.boot.SpringApplication;
import org.springframework.context.ApplicationContext;

public final class QuitFunction extends BuiltInFunctionStructImpl {

	private final ApplicationContext context;

	public QuitFunction(final ApplicationContext context) {
		super("Quits the JCL Application.",
		      CommonLispSymbols.QUIT.getName(),
		      Parameters.forFunction(CommonLispSymbols.QUIT.getName())
		);
		this.context = context;
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.QUIT;
	}

	@SuppressWarnings("all")
	@Override
	public LispStruct apply(final Arguments arguments) {
		System.exit(SpringApplication.exit(context));
		return null;
	}
}
