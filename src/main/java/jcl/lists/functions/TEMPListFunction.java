/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lists.functions;

import java.util.List;

import jcl.LispStruct;
import jcl.functions.BuiltInFunctionStruct;
import jcl.functions.parameterdsl.FunctionParameters;
import jcl.functions.parameterdsl.Parameters;
import jcl.lists.ListStruct;
import org.springframework.stereotype.Component;

@Component
public final class TEMPListFunction extends BuiltInFunctionStruct {

	public TEMPListFunction() {
		super("Returns a list containing the supplied objects.", "TEMP-LIST");
	}

	@Override
	public LispStruct apply(final FunctionParameters params) {
		final List<LispStruct> objects = params.getRestParameter();
		return ListStruct.buildProperList(objects);
	}

	@Override
	protected FunctionParameters getParams(final List<LispStruct> lispStructs) {
		return Parameters.forFunction("TEMP-LIST")
		                 .restParameter()
		                 .build(lispStructs);
	}
}
