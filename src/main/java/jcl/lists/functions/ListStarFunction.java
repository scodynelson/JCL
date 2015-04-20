/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lists.functions;

import java.util.Collections;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.binding.lambdalist.AuxBinding;
import jcl.compiler.real.environment.binding.lambdalist.KeyBinding;
import jcl.compiler.real.environment.binding.lambdalist.OptionalBinding;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.environment.binding.lambdalist.RequiredBinding;
import jcl.compiler.real.environment.binding.lambdalist.RestBinding;
import jcl.functions.FunctionStruct;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public final class ListStarFunction extends FunctionStruct {

	public static final SymbolStruct<?> LIST_STAR = new SymbolStruct<>("LIST*", GlobalPackageStruct.COMMON_LISP);

	private static final long serialVersionUID = -5968990222195634426L;

	private ListStarFunction() {
		super("Returns a list containing the supplied objects where the last argument becomes the cdr of the last cons constructed.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		LIST_STAR.setFunction(this);
	}

	private static OrdinaryLambdaListBindings getInitLambdaListBindings() {

		final List<RequiredBinding> requiredBindings = Collections.emptyList();
		final List<OptionalBinding> optionalBindings = Collections.emptyList();

		final SymbolStruct<?> objectRestArgSymbol = new SymbolStruct<>("OBJECTS", GlobalPackageStruct.COMMON_LISP);
		final RestBinding restBinding = new RestBinding(objectRestArgSymbol);

		final List<KeyBinding> keyBindings = Collections.emptyList();
		final boolean allowOtherKeys = false;
		final List<AuxBinding> auxBindings = Collections.emptyList();

		return new OrdinaryLambdaListBindings(requiredBindings, optionalBindings, restBinding, keyBindings, auxBindings, allowOtherKeys);
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		return listStar(lispStructs);
	}

	public LispStruct listStar(final LispStruct... lispStructs) {
		if (lispStructs.length == 1) {
			return lispStructs[0];
		}
		return ListStruct.buildDottedList(lispStructs);
	}
}
