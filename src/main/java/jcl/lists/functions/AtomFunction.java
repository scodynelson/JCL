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
import jcl.lists.ConsStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.BooleanStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import jcl.symbols.TStruct;
import org.springframework.stereotype.Component;

@Component
public final class AtomFunction extends FunctionStruct {

	public static final SymbolStruct<?> ATOM = new SymbolStruct<>("ATOM", GlobalPackageStruct.COMMON_LISP);

	private static final long serialVersionUID = 2155686329445366209L;

	private AtomFunction() {
		super("Returns T if object is of type atom; otherwise, returns NIL.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		ATOM.setFunction(this);
	}

	private static OrdinaryLambdaListBindings getInitLambdaListBindings() {

		final SymbolStruct<?> listArgSymbol = new SymbolStruct<>("OBJECT", GlobalPackageStruct.COMMON_LISP);
		final RequiredBinding requiredBinding = new RequiredBinding(listArgSymbol);
		final List<RequiredBinding> requiredBindings = Collections.singletonList(requiredBinding);

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

		return atom(lispStructs[0]);
	}

	public BooleanStruct atom(final LispStruct object) {
		return (object instanceof ConsStruct) ? NILStruct.INSTANCE : TStruct.INSTANCE;
	}
}
