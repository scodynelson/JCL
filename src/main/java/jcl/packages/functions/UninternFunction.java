/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages.functions;

import jcl.LispStruct;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.packages.PackageStruct;
import jcl.packages.PackageVariables;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import jcl.symbols.TStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code unintern}.
 */
@Component
public final class UninternFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "UNINTERN";
	private static final String SYMBOL_ARGUMENT = "SYMBOL";
	private static final String PACKAGE_ARGUMENT = "PACKAGE";

	/**
	 * Public constructor passing the documentation string.
	 */
	public UninternFunction() {
		super("Removes symbol from package.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(SYMBOL_ARGUMENT)
		                .optionalParameter(PACKAGE_ARGUMENT).withInitialValue(PackageVariables.PACKAGE.getVariableValue())
		);
	}

	/**
	 * {@inheritDoc}
	 * Application method for {@code unintern} package function that uninterns the provided {@link SymbolStruct} from
	 * an optionally provided {@link PackageStruct}.
	 *
	 * @param lispStructs
	 * 		the function parameters
	 *
	 * @return {@link TStruct#INSTANCE} if the {@link SymbolStruct} was successfully uninterned; {@link
	 * NILStruct#INSTANCE} otherwise
	 */
	@Override
	public LispStruct apply(final Arguments arguments) {
		final SymbolStruct symbol = arguments.getRequiredArgument(SYMBOL_ARGUMENT, SymbolStruct.class);
		final PackageStruct aPackage = arguments.getRequiredArgument(PACKAGE_ARGUMENT, PackageStruct.class);

		final boolean wasUninterned = aPackage.unintern(symbol);
		return wasUninterned ? TStruct.INSTANCE : NILStruct.INSTANCE;
	}
}
