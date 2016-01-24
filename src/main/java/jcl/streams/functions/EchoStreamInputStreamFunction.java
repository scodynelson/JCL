/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams.functions;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.streams.EchoStreamStruct;
import jcl.types.EchoStreamType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class EchoStreamInputStreamFunction extends AbstractCommonLispFunctionStruct {

	@Autowired
	private TypeValidator validator;

	public EchoStreamInputStreamFunction() {
		super("Returns the input stream from which echo-stream receives input.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "ECHO-STREAM").buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		validator.validateTypes(lispStruct, functionName(), "Echo Stream", EchoStreamType.INSTANCE);

		final EchoStreamStruct echoStream = (EchoStreamStruct) lispStruct;
		return echoStream.getInputStream();
	}

	@Override
	protected String functionName() {
		return "ECHO-STREAM-INPUT-STREAM";
	}
}
