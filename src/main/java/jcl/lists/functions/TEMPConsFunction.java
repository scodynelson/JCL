/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lists.functions;

import java.util.List;

import jcl.LispStruct;
import jcl.functions.BuiltInFunctionStruct;
import jcl.functions.parameterdsl.FunctionParameters;
import jcl.functions.parameterdsl.Parameters;
import jcl.lists.ConsStruct;
import org.springframework.stereotype.Component;

@Component
public final class TEMPConsFunction extends BuiltInFunctionStruct {

	public TEMPConsFunction() {
		super("Creates a fresh cons, the car of which is object-1 and the cdr of which is object-2.", "TEMP-CONS");
	}

	@Override
	public LispStruct apply(final FunctionParameters params) {
		final LispStruct object1 = params.getRequiredParameters().get("object1");
		final LispStruct object2 = params.getRequiredParameters().get("object2");
		return new ConsStruct(object1, object2);
	}

	@Override
	protected FunctionParameters getParams(final List<LispStruct> lispStructs) {
		return Parameters.forFunction("TEMP-CONS")
		                 .withParameters(lispStructs)
		                 .requiringAtLeast(2)
		                 .requiredParameter("object1").as(LispStruct.class)
		                 .requiredParameter("object2").as(LispStruct.class)
		                 .build();
	}
}
