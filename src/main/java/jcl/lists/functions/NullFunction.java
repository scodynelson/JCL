/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lists.functions;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.lists.NullStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.BooleanStructs;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public final class NullFunction extends AbstractCommonLispFunctionStruct {

	public static final SymbolStruct NULL = GlobalPackageStruct.COMMON_LISP.intern("NULL").getSymbol();

	public NullFunction() {
		super("Returns T if object is the empty list; otherwise, returns NIL.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "OBJECT").buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct object = lispStructs[0];
		return BooleanStructs.toLispBoolean(NullStruct.INSTANCE.equals(object) || NILStruct.INSTANCE.equals(object));
	}

	@Override
	protected String functionName() {
		return "NULL";
	}
}
