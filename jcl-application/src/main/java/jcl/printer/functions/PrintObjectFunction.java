/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.functions;

import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.statics.CommonLispSymbols;
import jcl.printer.Printer;
import lombok.extern.log4j.Log4j2;

@Log4j2
public final class PrintObjectFunction extends BuiltInFunctionStructImpl {

	private static final String OBJECT_ARGUMENT = "OBJECT";

	public PrintObjectFunction() {
		super("Prints the provided object.",
		      CommonLispSymbols.PRINT_OBJECT.getName(),
		      Parameters.forFunction(CommonLispSymbols.PRINT_OBJECT.getName())
		                .requiredParameter(OBJECT_ARGUMENT)
		);
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.PRINT_OBJECT;
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final LispStruct object = arguments.getRequiredArgument(OBJECT_ARGUMENT);

		final String printedObject = Printer.print(object);
		log.info(printedObject);
		return object;
	}
}
