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
import jcl.pathnames.PathnameStruct;
import jcl.pathnames.PathnameType;
import jcl.symbols.SymbolStruct;
import jcl.system.CommonLispSymbols;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class PathnameTypeFunction extends FunctionStruct {

	public static final SymbolStruct<?> PATHNAME_TYPE = new SymbolStruct<>("PATHNAME-TYPE", GlobalPackageStruct.COMMON_LISP);

	private static final long serialVersionUID = -4494745298812583275L;

	@Autowired
	private PathnameFunction pathnameFunction;

	private PathnameTypeFunction() {
		super("Returns the pathname-type component of the pathname denoted by pathspec.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		PATHNAME_TYPE.setFunction(this);
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
		final PathnameType pathnameType = pathnameType(pathspec);
		if (pathnameType == null) {
			return NullStruct.INSTANCE;
		}

		final String type = pathnameType.getType();
		final LispStruct returnValue;

		if (type == null) {
			final PathnameComponentType componentType = pathnameType.getComponentType();
			returnValue = componentType.getValue();
		} else {
			returnValue = new StringStruct(type);
		}

		return returnValue;
	}

	public PathnameType pathnameType(final LispStruct pathnameDesignator) {
		final PathnameStruct pathname = pathnameFunction.pathname(pathnameDesignator);
		return pathname.getPathnameType();
	}
}
