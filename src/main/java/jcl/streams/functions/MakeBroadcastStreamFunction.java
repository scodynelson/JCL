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
import jcl.streams.BroadcastStreamStruct;
import jcl.streams.OutputStream;
import jcl.types.StreamType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class MakeBroadcastStreamFunction extends AbstractCommonLispFunctionStruct {

	@Autowired
	private TypeValidator validator;

	@Autowired
	private Printer printer;

	public MakeBroadcastStreamFunction() {
		super("Returns a broadcast stream that has the indicated input-streams initially associated with it.");
	}

	@Override
	protected RestParameter getRestBinding() {
		return RestParameter.builder(GlobalPackageStruct.COMMON_LISP, "OUTPUT-STREAMS").build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final Deque<OutputStream> outputStreams = new ArrayDeque<>(lispStructs.length);
		for (final LispStruct lispStruct : lispStructs) {
			validator.validateTypes(lispStruct, functionName(), "Output Stream", StreamType.INSTANCE);
			if (!(lispStruct instanceof OutputStream)) {
				final String printedObject = printer.print(lispStruct);
				throw new TypeErrorException(functionName() + ": Output Stream must be an actual OUTPUT-STREAM. Got: " + printedObject);
			}
			outputStreams.add((OutputStream) lispStruct);
		}

		return new BroadcastStreamStruct(outputStreams);
	}

	@Override
	protected String functionName() {
		return "MAKE-BROADCAST-STREAM";
	}
}
