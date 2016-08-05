/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.pathname;

import jcl.lang.statics.CommonLispSymbols;
import jcl.lang.LispStruct;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.NILStruct;
import jcl.lang.pathname.PathnameComponentType;
import jcl.lang.pathname.PathnameStructImpl;
import jcl.lang.pathname.PathnameType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class PathnameTypeFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "PATHNAME-TYPE";
	private static final String PATHSPEC_ARGUMENT = "PATHSPEC";

	@Autowired
	private PathnameFunction pathnameFunction;

	public PathnameTypeFunction() {
		super("Returns the pathname-type component of the pathname denoted by pathspec.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(PATHSPEC_ARGUMENT)
		                .keyParameter(CommonLispSymbols.CASE_KEYWORD).withInitialValue(NILStruct.INSTANCE)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {

		final LispStruct pathspec = arguments.getRequiredArgument(PATHSPEC_ARGUMENT);
		final PathnameStructImpl pathname = pathnameFunction.pathname(pathspec);
		final PathnameType pathnameType = pathname.getPathnameType();
		if (pathnameType == null) {
			return NILStruct.INSTANCE;
		}

		final String type = pathnameType.getType();
		final LispStruct returnValue;

		if (type == null) {
			final PathnameComponentType componentType = pathnameType.getComponentType();
			returnValue = componentType.getValue();
		} else {
			returnValue = LispStructFactory.toString(type);
		}

		return returnValue;
	}
}
