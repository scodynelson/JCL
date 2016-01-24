/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams.functions;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.streams.SynonymStreamStruct;
import jcl.types.SynonymStreamType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class SynonymStreamSymbolFunction extends AbstractCommonLispFunctionStruct {

	@Autowired
	private TypeValidator validator;

	public SynonymStreamSymbolFunction() {
		super("Returns the symbol whose symbol-value the synonym-stream is using.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "SYNONYM-STREAM").buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		validator.validateTypes(lispStruct, functionName(), "Synonym Stream", SynonymStreamType.INSTANCE);

		final SynonymStreamStruct synonymStream = (SynonymStreamStruct) lispStruct;
		return synonymStream.getSymbol();
	}

	@Override
	protected String functionName() {
		return "SYNONYM-STREAM-SYMBOL";
	}
}
