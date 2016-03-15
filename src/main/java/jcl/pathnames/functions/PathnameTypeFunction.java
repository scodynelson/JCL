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
import jcl.packages.GlobalPackageStruct;
import jcl.pathnames.PathnameComponentType;
import jcl.pathnames.PathnameStruct;
import jcl.pathnames.PathnameType;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import jcl.system.CommonLispSymbols;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class PathnameTypeFunction extends FunctionStruct {

	public static final SymbolStruct PATHNAME_TYPE = GlobalPackageStruct.COMMON_LISP.intern("PATHNAME-TYPE").getSymbol();

	@Autowired
	private PathnameFunction pathnameFunction;

	private PathnameTypeFunction() {
		super("Returns the pathname-type component of the pathname denoted by pathspec.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		PATHNAME_TYPE.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(PATHNAME_TYPE);
	}

	private static OrdinaryLambdaList getInitLambdaListBindings() {

		final SymbolStruct pathspecArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("PATHSPEC").getSymbol();
		final RequiredParameter requiredBinding = new RequiredParameter(pathspecArgSymbol);
		final List<RequiredParameter> requiredBindings = Collections.singletonList(requiredBinding);

		final SymbolStruct caseArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("CASE").getSymbol();

		final SymbolStruct caseSuppliedPSymbol = GlobalPackageStruct.COMMON_LISP.intern("CASE-P-").getSymbol();
		final SuppliedPParameter suppliedPBinding = new SuppliedPParameter(caseSuppliedPSymbol);

		final KeyParameter keyBinding = new KeyParameter(caseArgSymbol, NILStruct.INSTANCE, CommonLispSymbols.CASE_KEYWORD, suppliedPBinding);
		final List<KeyParameter> keyBindings = Collections.singletonList(keyBinding);

		return OrdinaryLambdaList.builder()
		                         .requiredBindings(requiredBindings)
		                         .keyBindings(keyBindings)
		                         .build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {

		final LispStruct pathspec = lispStructs[0];
		final PathnameType pathnameType = pathnameType(pathspec);
		if (pathnameType == null) {
			return NILStruct.INSTANCE;
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
