/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.lisppackage;

import java.util.ArrayList;
import java.util.List;

import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.PackageStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.TStruct;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.statics.PackageVariables;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code export}.
 */
@Component
public final class ExportFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "EXPORT";
	private static final String SYMBOLS_ARGUMENT = "SYMBOLS";
	private static final String PACKAGE_ARGUMENT = "PACKAGE";

	/**
	 * Public constructor passing the documentation string.
	 */
	public ExportFunction() {
		super("Makes one or more symbols that are accessible in package (whether directly or by inheritance) be external symbols of that package.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(SYMBOLS_ARGUMENT)
		                .optionalParameter(PACKAGE_ARGUMENT).withInitialValue(PackageVariables.PACKAGE.getVariableValue())
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final LispStruct lispStruct = arguments.getRequiredArgument("SYMBOLS");
		final PackageStruct aPackage = PackageStruct.toLispPackage(arguments.getOptionalArgument("PACKAGE"));

		final SymbolStruct[] realSymbolArray;
		if (lispStruct instanceof ListStruct) {
			final ListStruct symbols = (ListStruct) lispStruct;
			final List<SymbolStruct> realSymbols = new ArrayList<>();
			for (final LispStruct theSymbol : symbols) {
				if (theSymbol instanceof SymbolStruct) {
					realSymbols.add((SymbolStruct) theSymbol);
				} else {
					throw new TypeErrorException("Cannot convert value '" + theSymbol + "' to type 'SYMBOL'");
				}
			}
			realSymbolArray = realSymbols.toArray(new SymbolStruct[realSymbols.size()]);
		} else if (lispStruct instanceof SymbolStruct) {
			realSymbolArray = new SymbolStruct[1];
			realSymbolArray[0] = (SymbolStruct) lispStruct;
		} else {
			throw new TypeErrorException("UNCAUGHT TYPE ERROR.");
		}

		aPackage.export(realSymbolArray);

		return TStruct.INSTANCE;
	}
}
