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
import jcl.symbols.BooleanStructs;
import jcl.types.StreamType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class InputStreamPFunction extends AbstractCommonLispFunctionStruct {

	private static final long serialVersionUID = 4693310104197305455L;

	@Autowired
	private TypeValidator validator;

	public InputStreamPFunction() {
		super("Returns true if stream is an input stream; otherwise, returns false.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "STREAM").buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		validator.validateTypes(lispStruct, functionName(), "STREAM", StreamType.INSTANCE);

		final StreamStruct stream = (StreamStruct) lispStruct;
		return BooleanStructs.toLispBoolean(stream.isInputStream());
	}

	@Override
	protected String functionName() {
		return "INPUT-STREAM-P";
	}
}
