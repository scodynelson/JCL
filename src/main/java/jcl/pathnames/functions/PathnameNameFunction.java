/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.pathnames.functions;

import java.util.Collections;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.compiler.environment.binding.lambdalist.KeyParameter;
import jcl.compiler.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.compiler.environment.binding.lambdalist.SuppliedPParameter;
import jcl.functions.FunctionStruct;
import jcl.lists.NullStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.pathnames.PathnameComponentType;
import jcl.pathnames.PathnameName;
import jcl.pathnames.PathnameStruct;
import jcl.symbols.SymbolStruct;
import jcl.system.CommonLispSymbols;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class PathnameNameFunction extends FunctionStruct {

	public static final SymbolStruct PATHNAME_NAME = GlobalPackageStruct.COMMON_LISP.intern("PATHNAME-NAME").getSymbol();

	private static final long serialVersionUID = 7837037227082426316L;

	@Autowired
	private PathnameFunction pathnameFunction;

	private PathnameNameFunction() {
		super("Returns the pathname-name component of the pathname denoted by pathspec.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		PATHNAME_NAME.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(PATHNAME_NAME);
	}

	private static OrdinaryLambdaList getInitLambdaListBindings() {

		final SymbolStruct pathspecArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("PATHSPEC").getSymbol();
		final RequiredParameter requiredBinding = new RequiredParameter(pathspecArgSymbol);
		final List<RequiredParameter> requiredBindings = Collections.singletonList(requiredBinding);

		final SymbolStruct caseArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("CASE").getSymbol();

		final SymbolStruct caseSuppliedPSymbol = GlobalPackageStruct.COMMON_LISP.intern("CASE-P-").getSymbol();
		final SuppliedPParameter suppliedPBinding = new SuppliedPParameter(caseSuppliedPSymbol);

		final KeyParameter keyBinding = new KeyParameter(caseArgSymbol, NullStruct.INSTANCE, CommonLispSymbols.CASE_KEYWORD, suppliedPBinding);
		final List<KeyParameter> keyBindings = Collections.singletonList(keyBinding);

		return new OrdinaryLambdaList.Builder().requiredBindings(requiredBindings)
		                                               .keyBindings(keyBindings)
		                                               .build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final LispStruct pathspec = lispStructs[0];
		final PathnameName pathnameName = pathnameName(pathspec);
		if (pathnameName == null) {
			return NullStruct.INSTANCE;
		}

		final String name = pathnameName.getName();
		final LispStruct returnValue;

		if (name == null) {
			final PathnameComponentType componentType = pathnameName.getComponentType();
			returnValue = componentType.getValue();
		} else {
			returnValue = new StringStruct(name);
		}

		return returnValue;
	}

	public PathnameName pathnameName(final LispStruct pathnameDesignator) {
		final PathnameStruct pathname = pathnameFunction.pathname(pathnameDesignator);
		return pathname.getPathnameName();
	}
}
