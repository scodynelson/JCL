/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions;

import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.symbols.SymbolStruct;

public abstract class AbstractCommonLispFunctionStruct extends FunctionStruct {

	private static final long serialVersionUID = 4937708575332457902L;

	protected AbstractCommonLispFunctionStruct(final String documentation) {
		super(documentation);
		initLambdaListBindings();
	}

	@Override
	public SymbolStruct<?> getFunctionSymbol() {
		final PackageStruct aPackage = GlobalPackageStruct.COMMON_LISP;
		final SymbolStruct<?> symbol = aPackage.intern(functionName()).getSymbol();
		aPackage.export(symbol);
		return symbol;
	}

	protected abstract String functionName();
}
