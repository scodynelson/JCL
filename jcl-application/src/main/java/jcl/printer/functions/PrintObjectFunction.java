/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.functions;

import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.LispStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.printer.Printer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class PrintObjectFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "PRINT-OBJECT";
	private static final String OBJECT_ARGUMENT = "OBJECT";

	private static final Logger LOGGER = LoggerFactory.getLogger(PrintObjectFunction.class);

	@Autowired
	private Printer printer;

	public PrintObjectFunction() {
		super("Prints the provided object.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(OBJECT_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final LispStruct object = arguments.getRequiredArgument(OBJECT_ARGUMENT);

		final String printedObject = printer.print(object);
		LOGGER.info(printedObject);
		return object;
	}
}
