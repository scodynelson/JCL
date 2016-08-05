/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.pathname;

import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.PathnameStruct;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.pathname.PathnameComponentType;
import jcl.lang.pathname.PathnameName;
import jcl.lang.statics.CommonLispSymbols;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class PathnameNameFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "PATHNAME-NAME";
	private static final String PATHSPEC_ARGUMENT = "PATHSPEC";

	@Autowired
	private PathnameFunction pathnameFunction;

	public PathnameNameFunction() {
		super("Returns the pathname-name component of the pathname denoted by pathspec.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(PATHSPEC_ARGUMENT)
		                .keyParameter(CommonLispSymbols.CASE_KEYWORD).withInitialValue(NILStruct.INSTANCE)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {

		final LispStruct pathspec = arguments.getRequiredArgument(PATHSPEC_ARGUMENT);
		final PathnameStruct pathname = pathnameFunction.pathname(pathspec);
		final PathnameName pathnameName = pathname.getPathnameName();
		if (pathnameName == null) {
			return NILStruct.INSTANCE;
		}

		final String name = pathnameName.getName();
		final LispStruct returnValue;

		if (name == null) {
			final PathnameComponentType componentType = pathnameName.getComponentType();
			returnValue = componentType.getValue();
		} else {
			returnValue = LispStructFactory.toString(name);
		}

		return returnValue;
	}
}
