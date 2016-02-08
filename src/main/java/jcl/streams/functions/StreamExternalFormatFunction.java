/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams.functions;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.streams.FileStreamStruct;
import jcl.types.StreamType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class StreamExternalFormatFunction extends AbstractCommonLispFunctionStruct {

	@Autowired
	private TypeValidator validator;

	public StreamExternalFormatFunction() {
		super("Returns an external file format designator for the stream.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "FILE-STREAM").buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final FileStreamStruct fileStream
				= validator.validateType(lispStructs[0], functionName(), "File Stream", StreamType.INSTANCE, FileStreamStruct.class);
		return fileStream.getExternalFormat();
	}

	@Override
	protected String functionName() {
		return "STREAM-EXTERNAL-FORMAT";
	}
}
