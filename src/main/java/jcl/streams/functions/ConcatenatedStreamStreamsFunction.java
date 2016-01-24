/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams.functions;

import java.util.ArrayList;
import java.util.Deque;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.streams.ConcatenatedStreamStruct;
import jcl.streams.InputStream;
import jcl.types.ConcatenatedStreamType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class ConcatenatedStreamStreamsFunction extends AbstractCommonLispFunctionStruct {

	@Autowired
	private TypeValidator validator;

	public ConcatenatedStreamStreamsFunction() {
		super("Returns a list of input streams that constitute the ordered set of streams the concatenated-stream still has to read from, starting with the current one it is reading from.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "CONCATENATED-STREAM").buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		validator.validateTypes(lispStruct, functionName(), "Concatenated Stream", ConcatenatedStreamType.INSTANCE);

		final ConcatenatedStreamStruct concatenatedStream = (ConcatenatedStreamStruct) lispStruct;
		final Deque<InputStream> inputStreams = concatenatedStream.getInputStreams();
		return ListStruct.buildProperList(new ArrayList<LispStruct>(inputStreams));
	}

	@Override
	protected String functionName() {
		return "CONCATENATED-STREAM-STREAMS";
	}
}
