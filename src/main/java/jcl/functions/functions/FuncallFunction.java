/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.functions;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

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
import jcl.symbols.SymbolStruct;

public class FuncallFunction extends FunctionStruct {

	public static final FuncallFunction INSTANCE = new FuncallFunction();

	public static final SymbolStruct<?> FUNCALL = new SymbolStruct<>("FUNCALL", GlobalPackageStruct.COMMON_LISP, null, INSTANCE);

	private static final long serialVersionUID = -1425587290881971372L;

	private FuncallFunction() {
		super("Applies function to args.", getInitLambdaListBindings());
	}

	private static OrdinaryLambdaListBindings getInitLambdaListBindings() {

		final SymbolStruct<?> functionArgSymbol = new SymbolStruct<>("function", GlobalPackageStruct.COMMON_LISP);
		final ParameterAllocation functionArgAllocation = new ParameterAllocation(0);
		final RequiredBinding requiredBinding = new RequiredBinding(functionArgSymbol, functionArgAllocation);
		final List<RequiredBinding> requiredBindings = Collections.singletonList(requiredBinding);

		final List<OptionalBinding> optionalBindings = Collections.emptyList();

		final SymbolStruct<?> argsArgSymbol = new SymbolStruct<>("args", GlobalPackageStruct.COMMON_LISP);
		final ParameterAllocation argsArgAllocation = new ParameterAllocation(1);
		final RestBinding restBinding = new RestBinding(argsArgSymbol, argsArgAllocation);

		final List<KeyBinding> keyBindings = Collections.emptyList();
		final boolean allowOtherKeys = false;
		final List<AuxBinding> auxBindings = Collections.emptyList();

		return new OrdinaryLambdaListBindings(requiredBindings, optionalBindings, restBinding, keyBindings, auxBindings, allowOtherKeys);
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		final List<LispStruct> lispStructsAsList = Arrays.asList(lispStructs);

		final LispStruct functionDesignator = lispStructsAsList.get(0);
		FunctionStruct functionStruct = null;
		if (functionDesignator instanceof SymbolStruct) {
			functionStruct = ((SymbolStruct) functionDesignator).getFunction();
		} else if (functionDesignator instanceof FunctionStruct) {
			functionStruct = (FunctionStruct) functionDesignator;
		}

		final List<LispStruct> args = lispStructsAsList.subList(1, lispStructsAsList.size());
		final ListStruct argsAsListStruct = ListStruct.buildProperList(args);

		if (functionStruct == null) {
			// TODO: print this???
			throw new ErrorException("Undefined function " + functionDesignator + " called with arguments " + argsAsListStruct);
		}

		return ApplyFunction.INSTANCE.apply(functionDesignator, argsAsListStruct);
	}
}
