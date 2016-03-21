/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.pathnames.functions;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.pathnames.PathnameComponentType;
import jcl.pathnames.PathnameStruct;
import jcl.pathnames.PathnameType;
import jcl.symbols.NILStruct;
import jcl.system.CommonLispSymbols;
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
		final PathnameStruct pathname = pathnameFunction.pathname(pathspec);
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
			returnValue = new StringStruct(type);
		}

		return returnValue;
	}
}
