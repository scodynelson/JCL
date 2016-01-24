/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams.functions;

import java.util.List;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.printer.Printer;
import jcl.streams.StringOutputStreamStruct;
import jcl.types.StringStreamType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class GetOutputStreamStringFunction extends AbstractCommonLispFunctionStruct {

	@Autowired
	private TypeValidator validator;

	@Autowired
	private Printer printer;

	public GetOutputStreamStringFunction() {
		super("Returns a string containing, in order, all the characters that have been output to string-output-stream.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "STRING-OUTPUT-STREAM").buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		validator.validateTypes(lispStruct, functionName(), "String Stream", StringStreamType.INSTANCE);

		if (!(lispStruct instanceof StringOutputStreamStruct)) {
			final String printedObject = printer.print(lispStruct);
			throw new TypeErrorException(functionName() + ": String Stream must be an actual STRING-OUTPUT-STREAM. Got: " + printedObject);
		}

		final StringOutputStreamStruct stringOutputStream = (StringOutputStreamStruct) lispStruct;
		final String streamString = stringOutputStream.getStreamString();
		stringOutputStream.clearStream();

		return new StringStruct(streamString);
	}

	@Override
	protected String functionName() {
		return "GET-OUTPUT-STREAM-STRING";
	}
}
