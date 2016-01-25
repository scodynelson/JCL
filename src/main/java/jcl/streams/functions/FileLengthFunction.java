/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams.functions;

import java.math.BigInteger;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.streams.StreamStruct;
import jcl.symbols.NILStruct;
import jcl.types.StreamType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class FileLengthFunction extends AbstractCommonLispFunctionStruct {

	@Autowired
	private TypeValidator validator;

	public FileLengthFunction() {
		super("Returns the length of stream, or nil if the length cannot be determined.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "STREAM").buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		validator.validateTypes(lispStruct, functionName(), "Stream", StreamType.INSTANCE);

		final StreamStruct streamStruct = (StreamStruct) lispStruct;
		final Long fileLength = streamStruct.fileLength();
		if (fileLength == null) {
			return NILStruct.INSTANCE;
		} else {
			return new IntegerStruct(BigInteger.valueOf(fileLength));
		}
	}

	@Override
	protected String functionName() {
		return "FILE-LENGTH";
	}
}
