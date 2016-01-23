/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams.functions;

import java.util.ArrayDeque;
import java.util.Deque;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.RestParameter;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.printer.Printer;
import jcl.streams.ConcatenatedStreamStruct;
import jcl.streams.InputStream;
import jcl.types.StreamType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class MakeConcatenatedStreamFunction extends AbstractCommonLispFunctionStruct {

	@Autowired
	private TypeValidator validator;

	@Autowired
	private Printer printer;

	public MakeConcatenatedStreamFunction() {
		super("Returns a concatenated stream that has the indicated input-streams initially associated with it.");
	}

	@Override
	protected RestParameter getRestBinding() {
		return RestParameter.builder(GlobalPackageStruct.COMMON_LISP, "INPUT-STREAMS").build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final Deque<InputStream> inputStreams = new ArrayDeque<>(lispStructs.length);
		for (final LispStruct lispStruct : lispStructs) {
			validator.validateTypes(lispStruct, functionName(), "Input Stream", StreamType.INSTANCE);
			if (!(lispStruct instanceof InputStream)) {
				final String printedObject = printer.print(lispStruct);
				throw new TypeErrorException(functionName() + ": Input Stream must be an actual INPUT-STREAM. Got: " + printedObject);
			}
			inputStreams.add((InputStream) lispStruct);
		}

		return new ConcatenatedStreamStruct(inputStreams);
	}

	@Override
	protected String functionName() {
		return "MAKE-CONCATENATED-STREAM";
	}
}
