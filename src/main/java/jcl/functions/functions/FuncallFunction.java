/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.functions;

import java.util.Arrays;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.compiler.environment.binding.lambdalist.RestParameter;
import jcl.compiler.struct.ValuesStruct;
import jcl.conditions.exceptions.ErrorException;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.functions.FunctionStruct;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.printer.Printer;
import jcl.symbols.SymbolStruct;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class FuncallFunction extends AbstractCommonLispFunctionStruct {

	private static final Logger LOGGER = LoggerFactory.getLogger(FuncallFunction.class);

	@Autowired
	private Printer printer;

	public FuncallFunction() {
		super("Applies function to args.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "FN").buildList();
	}

	@Override
	protected RestParameter getRestBinding() {
		return RestParameter.builder(GlobalPackageStruct.COMMON_LISP, "ARGS").build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		/* TRACING FUNCALL */
//		final String collect =
//				Arrays.stream(lispStructs)
//				      .map(lispStruct -> printer.print(lispStruct))
//				      .collect(Collectors.joining(" "));
//		LOGGER.error("0> Calling (FUNCALL {}", collect);
		/* TRACING FUNCALL */

		final List<LispStruct> lispStructsAsList = Arrays.asList(lispStructs);

		LispStruct functionDesignator = lispStructsAsList.get(0);
		if (functionDesignator instanceof ValuesStruct) {
			final ValuesStruct values = (ValuesStruct) functionDesignator;
			functionDesignator = values.getPrimaryValue();
		}

		FunctionStruct functionStruct = null;
		if (functionDesignator instanceof SymbolStruct) {
			functionStruct = ((SymbolStruct) functionDesignator).getFunction();
		} else if (functionDesignator instanceof FunctionStruct) {
			functionStruct = (FunctionStruct) functionDesignator;
		}

		final List<LispStruct> args = lispStructsAsList.subList(1, lispStructsAsList.size());
		final ListStruct argsAsListStruct = ListStruct.buildProperList(args);

		if (functionStruct == null) {
			final String printedFunctionDesignator = printer.print(functionDesignator);
			final String printedArguments = printer.print(argsAsListStruct);
			throw new ErrorException("Undefined function " + printedFunctionDesignator + " called with arguments " + printedArguments);
		}

		final LispStruct result = ApplyFunction.INSTANCE.apply(functionDesignator, argsAsListStruct);

		/* TRACING FUNCALL */
//		LOGGER.error("<0 FUNCALL returned {}", printer.print(result));
		/* TRACING FUNCALL */

		return result;
	}

	@Override
	protected String functionName() {
		return "FUNCALL";
	}
}
