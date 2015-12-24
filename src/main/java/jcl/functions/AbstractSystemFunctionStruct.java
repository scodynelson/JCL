/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions;

import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.symbols.SymbolStruct;

public abstract class AbstractSystemFunctionStruct extends AbstractCommonLispFunctionStruct {

	private static final long serialVersionUID = -3265981936110181436L;

	protected AbstractSystemFunctionStruct(final String documentation) {
		super(documentation);
		initLambdaListBindings();
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		final PackageStruct aPackage = GlobalPackageStruct.SYSTEM;
		final SymbolStruct symbol = aPackage.intern(functionName()).getSymbol();
		aPackage.export(symbol);
		return symbol;
	}
}
