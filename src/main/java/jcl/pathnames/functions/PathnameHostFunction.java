/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.pathnames.functions;

import java.util.Collections;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.compiler.real.environment.binding.lambdalist.AuxBinding;
import jcl.compiler.real.environment.binding.lambdalist.KeyBinding;
import jcl.compiler.real.environment.binding.lambdalist.OptionalBinding;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.environment.binding.lambdalist.RequiredBinding;
import jcl.compiler.real.environment.binding.lambdalist.RestBinding;
import jcl.compiler.real.environment.binding.lambdalist.SuppliedPBinding;
import jcl.functions.FunctionStruct;
import jcl.lists.NullStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.pathnames.PathnameComponentType;
import jcl.pathnames.PathnameHost;
import jcl.pathnames.PathnameStruct;
import jcl.symbols.SymbolStruct;
import jcl.system.CommonLispSymbols;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class PathnameHostFunction extends FunctionStruct {

	public static final SymbolStruct<?> PATHNAME_HOST = new SymbolStruct<>("PATHNAME-HOST", GlobalPackageStruct.COMMON_LISP);

	private static final long serialVersionUID = 5305854243645678052L;

	@Autowired
	private PathnameFunction pathnameFunction;

	private PathnameHostFunction() {
		super("Returns the pathname-host component of the pathname denoted by pathspec.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		PATHNAME_HOST.setFunction(this);
	}

	private static OrdinaryLambdaListBindings getInitLambdaListBindings() {

		final SymbolStruct<?> pathspecArgSymbol = new SymbolStruct<>("PATHSPEC", GlobalPackageStruct.COMMON_LISP);
		final RequiredBinding requiredBinding = new RequiredBinding(pathspecArgSymbol);
		final List<RequiredBinding> requiredBindings = Collections.singletonList(requiredBinding);

		final List<OptionalBinding> optionalBindings = Collections.emptyList();

		final RestBinding restBinding = null;

		final SymbolStruct<?> caseArgSymbol = new SymbolStruct<>("CASE", GlobalPackageStruct.COMMON_LISP);

		final SymbolStruct<?> caseSuppliedPSymbol = new SymbolStruct<>("CASE-P-" + System.nanoTime(), GlobalPackageStruct.SYSTEM);
		final SuppliedPBinding suppliedPBinding = new SuppliedPBinding(caseSuppliedPSymbol);

		final KeyBinding keyBinding = new KeyBinding(caseArgSymbol, NullStruct.INSTANCE, CommonLispSymbols.CASE_KEYWORD, suppliedPBinding);
		final List<KeyBinding> keyBindings = Collections.singletonList(keyBinding);

		final boolean allowOtherKeys = false;
		final List<AuxBinding> auxBindings = Collections.emptyList();

		return new OrdinaryLambdaListBindings(requiredBindings, optionalBindings, restBinding, keyBindings, auxBindings, allowOtherKeys);
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final LispStruct pathspec = lispStructs[0];
		final PathnameHost pathnameHost = pathnameHost(pathspec);
		if (pathnameHost == null) {
			return NullStruct.INSTANCE;
		}

		final String host = pathnameHost.getHost();
		final LispStruct returnValue;

		if (host == null) {
			final PathnameComponentType componentType = pathnameHost.getComponentType();
			returnValue = componentType.getValue();
		} else {
			returnValue = new StringStruct(host);
		}

		return returnValue;
	}

	public PathnameHost pathnameHost(final LispStruct pathnameDesignator) {
		final PathnameStruct pathname = pathnameFunction.pathname(pathnameDesignator);
		return pathname.getPathnameHost();
	}
}
