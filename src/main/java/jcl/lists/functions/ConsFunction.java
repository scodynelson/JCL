/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lists.functions;

import java.util.ArrayList;
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
import jcl.functions.FunctionStruct;
import jcl.lists.ConsStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public final class ConsFunction extends FunctionStruct {

	public static final SymbolStruct<?> CONS = new SymbolStruct<>("CONS", GlobalPackageStruct.COMMON_LISP);

	private static final long serialVersionUID = 1242798660975184815L;

	private ConsFunction() {
		super("Creates a fresh cons, the car of which is object-1 and the cdr of which is object-2.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		CONS.setFunction(this);
	}

	private static OrdinaryLambdaListBindings getInitLambdaListBindings() {

		final List<RequiredBinding> requiredBindings = new ArrayList<>();

		final SymbolStruct<?> object1ArgSymbol = new SymbolStruct<>("OBJECT-1", GlobalPackageStruct.COMMON_LISP);
		final ParameterAllocation object1ArgAllocation = new ParameterAllocation(0);
		final RequiredBinding object1RequiredBinding = new RequiredBinding(object1ArgSymbol, object1ArgAllocation);
		requiredBindings.add(object1RequiredBinding);

		final SymbolStruct<?> object2ArgSymbol = new SymbolStruct<>("OBJECT-2", GlobalPackageStruct.COMMON_LISP);
		final ParameterAllocation object2ArgAllocation = new ParameterAllocation(1);
		final RequiredBinding object2RequiredBinding = new RequiredBinding(object2ArgSymbol, object2ArgAllocation);
		requiredBindings.add(object2RequiredBinding);

		final List<OptionalBinding> optionalBindings = Collections.emptyList();

		final RestBinding restBinding = null;

		final List<KeyBinding> keyBindings = Collections.emptyList();
		final boolean allowOtherKeys = false;
		final List<AuxBinding> auxBindings = Collections.emptyList();

		return new OrdinaryLambdaListBindings(requiredBindings, optionalBindings, restBinding, keyBindings, auxBindings, allowOtherKeys);
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		return cons(lispStructs[0], lispStructs[1]);
	}

	public LispStruct cons(final LispStruct object1, final LispStruct object2) {
		return new ConsStruct(object1, object2);
	}
}
