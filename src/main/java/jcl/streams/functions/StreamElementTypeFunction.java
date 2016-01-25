/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams.functions;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.streams.StreamStruct;
import jcl.types.StreamType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class StreamElementTypeFunction extends AbstractCommonLispFunctionStruct {

	@Autowired
	private TypeValidator validator;

	public StreamElementTypeFunction() {
		super("Returns a type specifier that indicates the types of objects that may be read from or written to stream.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "STREAM").buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		validator.validateTypes(lispStruct, functionName(), "Stream", StreamType.INSTANCE);

		final StreamStruct streamStruct = (StreamStruct) lispStruct;
		return streamStruct.getElementType();
	}

	@Override
	protected String functionName() {
		return "STREAM-ELEMENT-TYPE";
	}
}
