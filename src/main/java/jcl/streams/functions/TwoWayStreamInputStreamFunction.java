/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams.functions;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.streams.TwoWayStreamStruct;
import jcl.types.TwoWayStreamType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class TwoWayStreamInputStreamFunction extends AbstractCommonLispFunctionStruct {

	@Autowired
	private TypeValidator validator;

	public TwoWayStreamInputStreamFunction() {
		super("Returns the input stream from which two-way-stream receives input.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "TWO-WAY-STREAM").buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		validator.validateTypes(lispStruct, functionName(), "Two-Way Stream", TwoWayStreamType.INSTANCE);

		final TwoWayStreamStruct twoWayStream = (TwoWayStreamStruct) lispStruct;
		return twoWayStream.getInputStream();
	}

	@Override
	protected String functionName() {
		return "TWO-WAY-STREAM-INPUT-STREAM";
	}
}
