/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.functions;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.allocation.ParameterAllocation;
import jcl.compiler.real.environment.binding.lambdalist.AuxBinding;
import jcl.compiler.real.environment.binding.lambdalist.KeyBinding;
import jcl.compiler.real.environment.binding.lambdalist.OptionalBinding;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.environment.binding.lambdalist.RequiredBinding;
import jcl.compiler.real.environment.binding.lambdalist.RestBinding;
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
public class ApplyFunction extends FunctionStruct {

	public static final ApplyFunction INSTANCE = new ApplyFunction();

	public static final SymbolStruct<?> APPLY = new SymbolStruct<>("APPLY", GlobalPackageStruct.COMMON_LISP, null, INSTANCE);

	private static final long serialVersionUID = 1994110477366960170L;

	@Autowired
	private Printer printer;

	private ApplyFunction() {
		super("Applies the function to the args.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		APPLY.setFunction(this);
	}

	private static OrdinaryLambdaListBindings getInitLambdaListBindings() {

		final List<RequiredBinding> requiredBindings = new ArrayList<>(2);

		final SymbolStruct<?> fnArgSymbol = new SymbolStruct<>("FN", GlobalPackageStruct.COMMON_LISP);
		final ParameterAllocation fnArgAllocation = new ParameterAllocation(0);
		final RequiredBinding functionRequiredBinding = new RequiredBinding(fnArgSymbol, fnArgAllocation);
		requiredBindings.add(functionRequiredBinding);

		final SymbolStruct<?> argArgSymbol = new SymbolStruct<>("ARG", GlobalPackageStruct.COMMON_LISP);
		final ParameterAllocation argArgAllocation = new ParameterAllocation(1);
		final RequiredBinding argRequiredBinding = new RequiredBinding(argArgSymbol, argArgAllocation);
		requiredBindings.add(argRequiredBinding);

		final List<OptionalBinding> optionalBindings = Collections.emptyList();

		final SymbolStruct<?> argsArgSymbol = new SymbolStruct<>("ARGS", GlobalPackageStruct.COMMON_LISP);
		final ParameterAllocation argsArgAllocation = new ParameterAllocation(2);
		final RestBinding restBinding = new RestBinding(argsArgSymbol, argsArgAllocation);

		final List<KeyBinding> keyBindings = Collections.emptyList();
		final boolean allowOtherKeys = false;
		final List<AuxBinding> auxBindings = Collections.emptyList();

		return new OrdinaryLambdaListBindings(requiredBindings, optionalBindings, restBinding, keyBindings, auxBindings, allowOtherKeys);
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

		final LispStruct functionDesignator = lispStructsAsList.get(0);
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
