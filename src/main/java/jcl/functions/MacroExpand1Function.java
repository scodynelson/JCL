/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions;

import jcl.compiler.environment.Environment;
import jcl.compiler.function.InternalMacroExpand;
import jcl.compiler.function.MacroExpandResult;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.statics.CommonLispSymbols;

public final class MacroExpand1Function extends BuiltInFunctionStructImpl {

	private static final String FORM_ARGUMENT = "FORM";
	private static final String ENVIRONMENT_ARGUMENT = "ENVIRONMENT";

	public MacroExpand1Function() {
		super("Expands form once.",
		      CommonLispSymbols.MACROEXPAND_1.getName(),
		      Parameters.forFunction(CommonLispSymbols.MACROEXPAND_1.getName())
		                .requiredParameter(FORM_ARGUMENT)
		                .optionalParameter(ENVIRONMENT_ARGUMENT).withInitialValue(Environment.NULL)
		);
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.MACROEXPAND_1;
	}

	@Override
	public LispStruct apply(final Arguments arguments) {

		final LispStruct form = arguments.getRequiredArgument(FORM_ARGUMENT);
		Environment environment = Environment.NULL;
		if (arguments.hasOptionalArgument(ENVIRONMENT_ARGUMENT)) {
			environment = arguments.getOptionalArgument(ENVIRONMENT_ARGUMENT, Environment.class);
		}

		final MacroExpandResult macroExpandResult = InternalMacroExpand.macroExpand1(form, environment);
		return macroExpandResult.toValues();
	}
}
