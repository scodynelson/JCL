/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.functions;

import java.util.Arrays;
import java.util.Collections;
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
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class FuncallFunction extends FunctionStruct {

	public static final FuncallFunction INSTANCE = new FuncallFunction();

	public static final SymbolStruct<?> FUNCALL = GlobalPackageStruct.COMMON_LISP.intern("FUNCALL").getSymbol();

	private static final long serialVersionUID = -1425587290881971372L;

	@Autowired
	private Printer printer;

	private FuncallFunction() {
		super("Applies function to args.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		FUNCALL.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(FUNCALL);
	}

	private static OrdinaryLambdaList getInitLambdaListBindings() {

		final SymbolStruct<?> fnArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("FN").getSymbol();
		final RequiredParameter requiredBinding = new RequiredParameter(fnArgSymbol);
		final List<RequiredParameter> requiredBindings = Collections.singletonList(requiredBinding);

		final SymbolStruct<?> argsArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("ARGS").getSymbol();
		final RestParameter restBinding = new RestParameter(argsArgSymbol);

		return new OrdinaryLambdaList.Builder().requiredBindings(requiredBindings)
		                                               .restBinding(restBinding)
		                                               .build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

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

		return ApplyFunction.INSTANCE.apply(functionDesignator, argsAsListStruct);
	}
}
