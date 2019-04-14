/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions;

import java.util.List;

import jcl.compiler.function.InternalApply;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

public final class FuncallFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "FUNCALL";
	private static final String FN_ARGUMENT = "FN";

	public FuncallFunction() {
		super("Applies function to args.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(FN_ARGUMENT)
		                .restParameter()
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final LispStruct functionDesignator = arguments.getRequiredArgument(FN_ARGUMENT);
		final List<LispStruct> functionArguments = arguments.getRestArgument();
		final ListStruct argumentList = ListStruct.toLispList(functionArguments);
		return InternalApply.funcall(functionDesignator, argumentList);
	}
}
