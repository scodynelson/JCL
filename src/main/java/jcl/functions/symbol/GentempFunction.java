/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.symbol;

import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.LispStruct;
import jcl.lang.PackageStruct;
import jcl.lang.StringStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.statics.CommonLispSymbols;

public final class GentempFunction extends BuiltInFunctionStructImpl {

	private static final String PREFIX_ARGUMENT = "PREFIX";
	private static final String PACKAGE_ARGUMENT = "PACKAGE";

	private int gentempCounter;

	public GentempFunction() {
		super("Creates and returns a fresh symbol, interned in the indicated package.",
		      CommonLispSymbols.GENTEMP.getName(),
		      Parameters.forFunction(CommonLispSymbols.GENTEMP.getName())
		                .optionalParameter(PREFIX_ARGUMENT).withInitialValue(StringStruct.toLispString("T"))
		                .optionalParameter(PACKAGE_ARGUMENT).withInitialValue(CommonLispSymbols.PACKAGE_VAR.getVariableValue())
		);
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.GENTEMP;
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final String prefix = arguments.getOptionalArgument(PREFIX_ARGUMENT, StringStruct.class).toJavaString();
		final PackageStruct aPackage = PackageStruct.fromDesignator(arguments.getOptionalArgument(PACKAGE_ARGUMENT));

		String symbolName = prefix + gentempCounter++;
		while (aPackage.findSymbol(symbolName).found()) {
			symbolName = prefix + gentempCounter++;
		}
		return aPackage.intern(symbolName).getSymbol();
	}
}
