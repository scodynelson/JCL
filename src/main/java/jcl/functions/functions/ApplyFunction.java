/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.functions;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.real.environment.binding.lambdalist.RequiredParameter;
import jcl.compiler.real.environment.binding.lambdalist.RestParameter;
import jcl.compiler.real.struct.ValuesStruct;
import jcl.conditions.exceptions.ErrorException;
import jcl.functions.FunctionStruct;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.printer.Printer;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class ApplyFunction extends FunctionStruct {

	public static final ApplyFunction INSTANCE = new ApplyFunction();

	public static final SymbolStruct<?> APPLY = GlobalPackageStruct.COMMON_LISP.intern("APPLY").getSymbol();

	private static final long serialVersionUID = 1994110477366960170L;

	@Autowired
	private Printer printer;

	private ApplyFunction() {
		super("Applies the function to the args.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		APPLY.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(APPLY);
	}

	private static OrdinaryLambdaList getInitLambdaListBindings() {

		final List<RequiredParameter> requiredBindings = new ArrayList<>(2);

		final SymbolStruct<?> fnArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("FN").getSymbol();
		final RequiredParameter functionRequiredBinding = new RequiredParameter(fnArgSymbol);
		requiredBindings.add(functionRequiredBinding);

		final SymbolStruct<?> argArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("ARG").getSymbol();
		final RequiredParameter argRequiredBinding = new RequiredParameter(argArgSymbol);
		requiredBindings.add(argRequiredBinding);

		final SymbolStruct<?> argsArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("ARGS").getSymbol();
		final RestParameter restBinding = new RestParameter(argsArgSymbol);

		return new OrdinaryLambdaList.Builder().requiredBindings(requiredBindings)
		                                               .restBinding(restBinding)
		                                               .build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final LispStruct lastArgument = lispStructs[lispStructs.length - 1];
		if (!(lastArgument instanceof ListStruct) && !NILStruct.INSTANCE.equals(lastArgument)) {
			final String printedObject = printer.print(lastArgument);
			throw new ErrorException("Can't construct argument list from " + printedObject + '.');
		}

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
		// NOTE: Build dotted list here since we've verified that the last element is a List.
		final ListStruct argsAsListStruct = ListStruct.buildDottedList(args);

		if (functionStruct == null) {
			final String printedFunctionDesignator = printer.print(functionDesignator);
			final String printedArguments = printer.print(argsAsListStruct);
			throw new ErrorException("Undefined function " + printedFunctionDesignator + " called with arguments " + printedArguments);
		}

		final LispStruct[] argsToApply = new LispStruct[argsAsListStruct.size()];

		final List<LispStruct> argsAsListStructAsJavaList = argsAsListStruct.getAsJavaList();
		for (int i = 0; i < argsAsListStructAsJavaList.size(); i++) {
			final LispStruct currentArg = argsAsListStructAsJavaList.get(i);
			argsToApply[i] = currentArg;
		}

		return functionStruct.apply(argsToApply);
	}
}
