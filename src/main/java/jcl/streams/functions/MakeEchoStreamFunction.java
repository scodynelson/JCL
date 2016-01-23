/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams.functions;

import java.util.ArrayList;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.printer.Printer;
import jcl.streams.EchoStreamStruct;
import jcl.streams.InputStream;
import jcl.streams.OutputStream;
import jcl.types.StreamType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class MakeEchoStreamFunction extends AbstractCommonLispFunctionStruct {

	@Autowired
	private TypeValidator validator;

	@Autowired
	private Printer printer;

	public MakeEchoStreamFunction() {
		super("Returns a echo stream that gets its input from input-stream and sends its output to output-stream.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		final List<RequiredParameter> requiredParameters = new ArrayList<>(2);

		final RequiredParameter inputStream =
				RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "INPUT-STREAM").build();
		requiredParameters.add(inputStream);

		final RequiredParameter outputStream =
				RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "OUTPUT-STREAM").build();
		requiredParameters.add(outputStream);

		return requiredParameters;
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct lispStruct1 = lispStructs[0];
		validator.validateTypes(lispStruct1, functionName(), "Input Stream", StreamType.INSTANCE);

		if (!(lispStruct1 instanceof InputStream)) {
			final String printedObject = printer.print(lispStruct1);
			throw new TypeErrorException(functionName() + ": Input Stream must be an actual INPUT-STREAM. Got: " + printedObject);
		}
		final InputStream inputStream = (InputStream) lispStruct1;

		final LispStruct lispStruct2 = lispStructs[1];
		validator.validateTypes(lispStruct2, functionName(), "Input Stream", StreamType.INSTANCE);

		if (!(lispStruct2 instanceof OutputStream)) {
			final String printedObject = printer.print(lispStruct2);
			throw new TypeErrorException(functionName() + ": Output Stream must be an actual OUTPUT-STREAM. Got: " + printedObject);
		}
		final OutputStream outputStream = (OutputStream) lispStruct2;

		return new EchoStreamStruct(inputStream, outputStream);
	}

	@Override
	protected String functionName() {
		return "MAKE-ECHO-STREAM";
	}
}
