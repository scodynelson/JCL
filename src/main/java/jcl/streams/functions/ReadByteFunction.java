/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams.functions;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.OptionalParameter;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.lists.NullStruct;
import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.printer.Printer;
import jcl.streams.InputStream;
import jcl.streams.ReadPeekResult;
import jcl.streams.StreamVariables;
import jcl.symbols.BooleanStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.TStruct;
import jcl.types.BooleanType;
import jcl.types.StreamType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class ReadByteFunction extends AbstractCommonLispFunctionStruct {

	@Autowired
	private TypeValidator validator;

	@Autowired
	private Printer printer;

	public ReadByteFunction() {
		super("Returns the next byte from input-stream.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "INPUT-STREAM").buildList();
	}

	@Override
	protected List<OptionalParameter> getOptionalBindings() {
		final List<OptionalParameter> optionalParameters = new ArrayList<>(2);

		final OptionalParameter eofErrorPOptionalBinding =
				OptionalParameter.builder(GlobalPackageStruct.COMMON_LISP, "EOF-ERROR")
				                 .suppliedPBinding()
				                 .initForm(TStruct.INSTANCE)
				                 .build();
		optionalParameters.add(eofErrorPOptionalBinding);
		final OptionalParameter eofValueOptionalBinding =
				OptionalParameter.builder(GlobalPackageStruct.COMMON_LISP, "EOF-VALUE")
				                 .suppliedPBinding()
				                 .initForm(NILStruct.INSTANCE)
				                 .build();
		optionalParameters.add(eofValueOptionalBinding);

		return optionalParameters;
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct inputStreamArg = lispStructs[0];
		validator.validateTypes(inputStreamArg, functionName(), "Input Stream", BooleanType.INSTANCE, StreamType.INSTANCE);

		final InputStream inputStream;
		if (TStruct.INSTANCE.equals(inputStreamArg)) {
			inputStream = StreamVariables.STANDARD_INPUT.getVariableValue();
		} else if (NILStruct.INSTANCE.equals(inputStreamArg) || NullStruct.INSTANCE.equals(inputStreamArg)) {
			inputStream = StreamVariables.STANDARD_INPUT.getVariableValue();
		} else if (inputStreamArg instanceof InputStream) {
			inputStream = (InputStream) inputStreamArg;
		} else {
			final String printedObject = printer.print(inputStreamArg);
			throw new TypeErrorException("The value " + printedObject + " is not either T, NIL, or an Input Stream.");
		}

		final int length = lispStructs.length;

		BooleanStruct eofErrorP = TStruct.INSTANCE;
		if (length > 1) {
			final LispStruct lispStruct = lispStructs[1];
			validator.validateTypes(lispStruct, functionName(), "EOF Error Predicate", BooleanType.INSTANCE);
			eofErrorP = (BooleanStruct) lispStruct;
		}

		LispStruct eofValue = NILStruct.INSTANCE;
		if (length > 2) {
			eofValue = lispStructs[2];
		}

		final ReadPeekResult readPeekResult = inputStream.readByte(eofErrorP.booleanValue(), eofValue);
		return readPeekResult.isEof() ? eofValue : new IntegerStruct(BigInteger.valueOf(readPeekResult.getResult()));
	}

	@Override
	protected String functionName() {
		return "READ-BYTE";
	}
}
