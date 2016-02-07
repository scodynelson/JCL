/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams.functions;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.printer.Printer;
import jcl.streams.OutputStream;
import jcl.types.IntegerType;
import jcl.types.StreamType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class WriteByteFunction extends AbstractCommonLispFunctionStruct {

	@Autowired
	private TypeValidator validator;

	@Autowired
	private Printer printer;

	public WriteByteFunction() {
		super("Outputs the byte to the output-stream.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		final List<RequiredParameter> requiredParameters = new ArrayList<>();

		final RequiredParameter byteParam = RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "BYTE").build();
		requiredParameters.add(byteParam);

		final RequiredParameter outputStreamParam = RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "OUTPUT-STREAM").build();
		requiredParameters.add(outputStreamParam);

		return requiredParameters;
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct lispStruct1 = lispStructs[0];
		validator.validateTypes(lispStruct1, functionName(), "Byte", IntegerType.INSTANCE);
		final IntegerStruct byteVal = (IntegerStruct) lispStruct1;

		final LispStruct lispStruct2 = lispStructs[1];
		validator.validateTypes(lispStruct2, functionName(), "Output Stream", StreamType.INSTANCE);

		if (!(lispStruct2 instanceof OutputStream)) {
			final String printedObject = printer.print(lispStruct2);
			throw new TypeErrorException(functionName() + ": Output Stream must be an actual OUTPUT-STREAM. Got: " + printedObject);
		}
		final OutputStream outputStream = (OutputStream) lispStruct2;

		final BigInteger bigInteger = byteVal.getBigInteger();
		outputStream.writeByte(bigInteger.intValue());

		return byteVal;
	}

	@Override
	protected String functionName() {
		return "WRITE-BYTE";
	}
}
