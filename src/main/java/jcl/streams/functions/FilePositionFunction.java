/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams.functions;

import java.math.BigInteger;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.OptionalParameter;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.lists.NullStruct;
import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.streams.StreamStruct;
import jcl.symbols.BooleanStructs;
import jcl.symbols.NILStruct;
import jcl.system.CommonLispSymbols;
import jcl.types.IntegerType;
import jcl.types.StreamType;
import jcl.types.SymbolType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class FilePositionFunction extends AbstractCommonLispFunctionStruct {

	@Autowired
	private TypeValidator validator;

	public FilePositionFunction() {
		super("Returns the length of stream, or nil if the length cannot be determined.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "STREAM").buildList();
	}

	@Override
	protected List<OptionalParameter> getOptionalBindings() {
		return OptionalParameter.builder(GlobalPackageStruct.COMMON_LISP, "POSITION")
		                        .suppliedPBinding()
		                        .initForm(NILStruct.INSTANCE)
		                        .buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct lispStruct1 = lispStructs[0];
		validator.validateTypes(lispStruct1, functionName(), "Stream", StreamType.INSTANCE);

		final LispStruct lispStruct2 = lispStructs[1];
		validator.validateTypes(lispStruct2, functionName(), "File Position", IntegerType.INSTANCE, SymbolType.INSTANCE);

		final StreamStruct stream = (StreamStruct) lispStruct1;

		final Long position;
		if (lispStruct2 instanceof IntegerStruct) {
			position = ((IntegerStruct) lispStruct2).getBigInteger().longValue();
		} else if (CommonLispSymbols.START_KEYWORD.equals(lispStruct2)) {
			position = 0L;
		} else if (CommonLispSymbols.END_KEYWORD.equals(lispStruct2)) {
			position = stream.fileLength();
		} else if (NILStruct.INSTANCE.equals(lispStruct2) || NullStruct.INSTANCE.equals(lispStruct2)) {
			position = null;
		} else {
			throw new TypeErrorException("UNCAUGHT TYPE ERROR.");
		}

		if (position == null) {
			final Long currentPosition = stream.filePosition(null);
			return new IntegerStruct(BigInteger.valueOf(currentPosition));
		} else {
			final Long newPosition = stream.filePosition(position);
			return BooleanStructs.toLispBoolean(newPosition != null);
		}
	}

	@Override
	protected String functionName() {
		return "FILE-POSITION";
	}
}
