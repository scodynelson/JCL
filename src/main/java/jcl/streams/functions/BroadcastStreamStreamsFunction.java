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
import jcl.streams.BroadcastStreamStruct;
import jcl.streams.OutputStream;
import jcl.types.BroadcastStreamType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class BroadcastStreamStreamsFunction extends AbstractCommonLispFunctionStruct {

	@Autowired
	private TypeValidator validator;

	public BroadcastStreamStreamsFunction() {
		super("Returns a list of output streams that constitute all the streams to which the broadcast-stream is broadcasting.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "BROADCAST-STREAM").buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		validator.validateTypes(lispStruct, functionName(), "Broadcast Stream", BroadcastStreamType.INSTANCE);

		final BroadcastStreamStruct broadcastStream = (BroadcastStreamStruct) lispStruct;
		final Deque<OutputStream> outputStreams = broadcastStream.getOutputStreams();
		return ListStruct.buildProperList(new ArrayList<LispStruct>(outputStreams));
	}

	@Override
	protected String functionName() {
		return "BROADCAST-STREAM-STREAMS";
	}
}
