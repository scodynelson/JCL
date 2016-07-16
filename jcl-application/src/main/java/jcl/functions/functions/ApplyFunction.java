/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.functions;

import java.util.List;

import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.FunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.printer.Printer;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class ApplyFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "APPLY";
	private static final String FN_ARGUMENT = "FN";
	private static final String ARG_ARGUMENT = "ARG";

	@Autowired
	private Printer printer;

	private ApplyFunction() {
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

		FunctionStruct functionStruct = null;
		if (functionDesignator instanceof SymbolStruct) {
			functionStruct = ((SymbolStruct) functionDesignator).getFunction();
		} else if (functionDesignator instanceof FunctionStruct) {
			functionStruct = (FunctionStruct) functionDesignator;
		}

		final LispStruct arg = arguments.getRequiredArgument(ARG_ARGUMENT);
		final List<LispStruct> args = arguments.getRestArgument();

		if (functionStruct == null) {
			final String printedFunctionDesignator = printer.print(functionDesignator);
//			final String printedArguments = printer.print(functionList);
			final String printedArguments = arg + " " + args;
			throw new ErrorException("Undefined function " + printedFunctionDesignator + " called with arguments " + printedArguments);
		}

		if (args.isEmpty()) {
			if (!(arg instanceof ListStruct)) {
				final String printedObject = printer.print(arg);
				throw new ErrorException("Can't construct argument list from " + printedObject + '.');
			}

			final ListStruct argAsList = (ListStruct) arg;
			final LispStruct[] lispStructs = argAsList.toArray();
			return functionStruct.apply(lispStructs);
		}
		final int argsSize = args.size();

		final LispStruct lastElement = args.get(argsSize - 1);
		if (!(lastElement instanceof ListStruct)) {
			final String printedObject = printer.print(lastElement);
			throw new ErrorException("Can't construct argument list from " + printedObject + '.');
		}
		final ListStruct lastElementList = (ListStruct) lastElement;
		final LispStruct[] lastElementArray = lastElementList.toArray();
		final int lastElementSize = lastElementArray.length;

		// NOTE: We add 1 for the required argument, but also subtract 1 for the last element being a list
		final LispStruct[] argumentsArrays = new LispStruct[argsSize + lastElementSize];
		argumentsArrays[0] = arg;
		for (int i = 0; i < (argsSize - 1); i++) {
			argumentsArrays[i + 1] = args.get(i);
		}
		System.arraycopy(lastElementArray, 0, argumentsArrays, argsSize, lastElementSize);

		return functionStruct.apply(argumentsArrays);
	}
}
