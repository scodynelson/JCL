/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.functions;

import java.util.List;

import jcl.LispStruct;
import jcl.conditions.exceptions.ErrorException;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.FunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.lists.ListStruct;
import jcl.printer.Printer;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class FuncallFunction extends CommonLispBuiltInFunctionStruct {

//	private static final Logger LOGGER = LoggerFactory.getLogger(FuncallFunction.class);

	private static final String FUNCTION_NAME = "FUNCALL";
	private static final String FN_ARGUMENT = "FN";

	@Autowired
	private ApplyFunction applyFunction;

	@Autowired
	private Printer printer;

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
		/* TRACING FUNCALL */
//		final String collect =
//				Arrays.stream(lispStructs)
//				      .map(lispStruct -> printer.print(lispStruct))
//				      .collect(Collectors.joining(" "));
//		LOGGER.error("0> Calling (FUNCALL {}", collect);
		/* TRACING FUNCALL */

		final LispStruct functionDesignator = arguments.getRequiredArgument(FN_ARGUMENT);

		FunctionStruct functionStruct = null;
		if (functionDesignator instanceof SymbolStruct) {
			functionStruct = ((SymbolStruct) functionDesignator).getFunction();
		} else if (functionDesignator instanceof FunctionStruct) {
			functionStruct = (FunctionStruct) functionDesignator;
		}

		final List<LispStruct> functionArguments = arguments.getRestArgument();
		final ListStruct argumentList = ListStruct.buildProperList(functionArguments);

		if (functionStruct == null) {
			final String printedFunctionDesignator = printer.print(functionDesignator);
			final String printedArguments = printer.print(argumentList);
			throw new ErrorException("Undefined function " + printedFunctionDesignator + " called with arguments " + printedArguments);
		}

		final LispStruct[] argumentsArrays = new LispStruct[2];
		argumentsArrays[0] = functionDesignator;
		argumentsArrays[1] = argumentList;

		final Parameters parameters = applyFunction.getParameters();
		final Arguments applyArguments = parameters.build(argumentsArrays);

		final LispStruct result = applyFunction.apply(applyArguments);

		/* TRACING FUNCALL */
//		LOGGER.error("<0 FUNCALL returned {}", printer.print(result));
		/* TRACING FUNCALL */

		return result;
	}
}
