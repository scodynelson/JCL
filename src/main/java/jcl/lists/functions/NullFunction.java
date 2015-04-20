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
import jcl.lists.NullStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.BooleanStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import jcl.symbols.TStruct;
import org.springframework.stereotype.Component;

@Component
public final class NullFunction extends FunctionStruct {

	public static final SymbolStruct<?> NULL = new SymbolStruct<>("NULL", GlobalPackageStruct.COMMON_LISP);

	private static final long serialVersionUID = 7471817337078296980L;

	private NullFunction() {
		super("Returns T if object is the empty list; otherwise, returns NIL.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		NULL.setFunction(this);
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

		return nullFn(lispStructs[0]);
	}

	public BooleanStruct nullFn(final LispStruct object) {
		return nullFnJavaBoolean(object) ? TStruct.INSTANCE : NILStruct.INSTANCE;
	}

	public boolean nullFnJavaBoolean(final LispStruct object) {
		return NullStruct.INSTANCE.equals(object) || NILStruct.INSTANCE.equals(object);
	}
}
