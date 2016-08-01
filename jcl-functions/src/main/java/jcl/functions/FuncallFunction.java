/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions;

import java.util.List;

import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.FunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.ListStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class FuncallFunction extends CommonLispBuiltInFunctionStruct {

//	private static final Logger LOGGER = LoggerFactory.getLogger(FuncallFunction.class);

	private static final String FUNCTION_NAME = "FUNCALL";
	private static final String FN_ARGUMENT = "FN";

	@Autowired
	private ApplyFunction applyFunction;

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
		final ListStruct argumentList = LispStructFactory.toProperList(functionArguments);

		if (functionStruct == null) {
			throw new ErrorException("Undefined function " + functionDesignator + " called with arguments " + argumentList);
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
