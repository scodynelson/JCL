/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.pathname;

import jcl.lang.CommonLispSymbols;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.array.StringStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.pathname.PathnameComponentType;
import jcl.lang.pathname.PathnameDevice;
import jcl.lang.pathname.PathnameStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class PathnameDeviceFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "PATHNAME-DEVICE";
	private static final String PATHSPEC_ARGUMENT = "PATHSPEC";

	@Autowired
	private PathnameFunction pathnameFunction;

	public PathnameDeviceFunction() {
		super("Returns the pathname-device component of the pathname denoted by pathspec.",
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
		final PathnameDevice pathnameDevice = pathname.getPathnameDevice();
		if (pathnameDevice == null) {
			return NILStruct.INSTANCE;
		}

		final String device = pathnameDevice.getDevice();
		final LispStruct returnValue;

		if (device == null) {
			final PathnameComponentType componentType = pathnameDevice.getComponentType();
			returnValue = componentType.getValue();
		} else {
			returnValue = new StringStruct(device);
		}

		return returnValue;
	}
}