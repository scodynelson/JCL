/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.pathnames.functions;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.pathnames.PathnameStruct;
import jcl.pathnames.PathnameVersion;
import jcl.pathnames.PathnameVersionComponentType;
import jcl.symbols.NILStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class PathnameVersionFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "PATHNAME-VERSION";
	private static final String PATHSPEC_ARGUMENT = "PATHSPEC";

	@Autowired
	private PathnameFunction pathnameFunction;

	public PathnameVersionFunction() {
		super("Returns the pathname-version component of the pathname denoted by pathspec.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(PATHSPEC_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {

		final LispStruct pathspec = arguments.getRequiredArgument(PATHSPEC_ARGUMENT);
		final PathnameStruct pathname = pathnameFunction.pathname(pathspec);
		final PathnameVersion pathnameVersion = pathname.getPathnameVersion();
		if (pathnameVersion == null) {
			return NILStruct.INSTANCE;
		}

		final Integer version = pathnameVersion.getVersion();
		final LispStruct returnValue;

		if (version == null) {
			final PathnameVersionComponentType componentType = pathnameVersion.getComponentType();
			returnValue = componentType.getValue();
		} else {
			final String versionString = version.toString();
			returnValue = new StringStruct(versionString);
		}

		return returnValue;
	}
}
