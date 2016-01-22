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
public final class InteractiveStreamPFunction extends AbstractCommonLispFunctionStruct {

	private static final long serialVersionUID = -7886789546416011880L;

	@Autowired
	private TypeValidator typeValidator;

	public InteractiveStreamPFunction() {
		super("Returns true if stream is an interactive stream; otherwise, returns false.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "STREAM").buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		typeValidator.validateTypes(lispStruct, functionName(), "STREAM", StreamType.INSTANCE);

		final StreamStruct stream = (StreamStruct) lispStruct;
		return BooleanStructs.toLispBoolean(stream.isInteractive());
	}

	@Override
	protected String functionName() {
		return "INTERACTIVE-STREAM-P";
	}
}
