/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions;

import jcl.compiler.function.InternalApply;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

public final class ApplyFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "APPLY";
	private static final String FN_ARGUMENT = "FN";
	private static final String ARG_ARGUMENT = "ARG";

	public ApplyFunction() {
		super("Applies the function to the args.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(FN_ARGUMENT)
		                .requiredParameter(ARG_ARGUMENT)
		                .restParameter()
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final LispStruct functionDesignator = arguments.getRequiredArgument(FN_ARGUMENT);
		final LispStruct arg = arguments.getRequiredArgument(ARG_ARGUMENT);
		return InternalApply.apply(functionDesignator, arg, ListStruct.toLispList(arguments.getRestArgument()));
	}
}
