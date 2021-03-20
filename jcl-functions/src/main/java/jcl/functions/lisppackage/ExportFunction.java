/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.lisppackage;

import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.PackageStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.TStruct;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.statics.CommonLispSymbols;
import jcl.lang.statics.PackageVariables;

/**
 * Function implementation for {@code export}.
 */
public final class ExportFunction extends BuiltInFunctionStructImpl {

	private static final String SYMBOLS_ARGUMENT = "SYMBOLS";
	private static final String PACKAGE_ARGUMENT = "PACKAGE";

	/**
	 * Public constructor passing the documentation string.
	 */
	public ExportFunction() {
		super("Makes one or more symbols that are accessible in package (whether directly or by inheritance) be external symbols of that package.",
		      CommonLispSymbols.EXPORT.getName(),
		      Parameters.forFunction(CommonLispSymbols.EXPORT.getName())
		                .requiredParameter(SYMBOLS_ARGUMENT)
		                .optionalParameter(PACKAGE_ARGUMENT).withInitialValue(PackageVariables.PACKAGE.getVariableValue())
		);
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.EXPORT;
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final LispStruct lispStruct = arguments.getRequiredArgument(SYMBOLS_ARGUMENT);
		final PackageStruct aPackage = PackageStruct.fromDesignator(arguments.getOptionalArgument(PACKAGE_ARGUMENT));

		if (lispStruct instanceof ListStruct) {
			aPackage.export((ListStruct) lispStruct);
		} else if (lispStruct instanceof SymbolStruct) {
			aPackage.export(ListStruct.toLispList(lispStruct));
		} else {
			throw new TypeErrorException("UNCAUGHT TYPE ERROR.");
		}

		return TStruct.INSTANCE;
	}
}
