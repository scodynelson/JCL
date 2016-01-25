/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams.functions;

import java.util.ArrayList;
import java.util.List;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.compiler.environment.binding.lambdalist.OptionalParameter;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.printer.Printer;
import jcl.streams.StringInputStreamStruct;
import jcl.symbols.NILStruct;
import jcl.types.IntegerType;
import jcl.types.NILType;
import jcl.types.NullType;
import jcl.types.StringType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class MakeStringInputStreamFunction extends AbstractCommonLispFunctionStruct {

	@Autowired
	private TypeValidator validator;

	@Autowired
	private Printer printer;

	public MakeStringInputStreamFunction() {
		super("Returns an input string stream.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "STRING").buildList();
	}

	@Override
	protected List<OptionalParameter> getOptionalBindings() {
		final List<OptionalParameter> optionalParameters = new ArrayList<>(2);

		final OptionalParameter start =
				OptionalParameter.builder(GlobalPackageStruct.COMMON_LISP, "START")
				                 .suppliedPBinding()
				                 .initForm(IntegerStruct.ZERO)
				                 .build();
		optionalParameters.add(start);

		final OptionalParameter end =
				OptionalParameter.builder(GlobalPackageStruct.COMMON_LISP, "END")
				                 .suppliedPBinding()
				                 .initForm(NILStruct.INSTANCE)
				                 .build();
		optionalParameters.add(end);

		return optionalParameters;
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		validator.validateTypes(lispStruct, functionName(), "String", StringType.INSTANCE);

		final StringStruct aString = (StringStruct) lispStruct;
		final String javaString = aString.getAsJavaString();

		int start = 0;
		int end = javaString.length();

		if (lispStructs.length > 1) {
			final LispStruct startParam = lispStructs[1];
			validator.validateTypes(startParam, functionName(), "Start", IntegerType.INSTANCE, NILType.INSTANCE, NullType.INSTANCE);

			if (startParam instanceof IntegerStruct) {
				final int possibleStartValue = ((IntegerStruct) startParam).getBigInteger().intValue();
				if (possibleStartValue > end) {
					final String printedObject = printer.print(startParam);
					throw new TypeErrorException(functionName() + ": Start value must be less than or equal to the length of the String. Got: " + printedObject);
				}
				start = possibleStartValue;
			}
		}

		if (lispStructs.length > 2) {
			final LispStruct endParam = lispStructs[2];
			validator.validateTypes(endParam, functionName(), "End", IntegerType.INSTANCE, NILType.INSTANCE, NullType.INSTANCE);

			if (endParam instanceof IntegerStruct) {
				final int possibleEndValue = ((IntegerStruct) endParam).getBigInteger().intValue();
				if (possibleEndValue > end) {
					final String printedObject = printer.print(endParam);
					throw new TypeErrorException(functionName() + ": End value must be less than or equal to the length of the String. Got: " + printedObject);
				}
				end = possibleEndValue;
			}
		}

		return new StringInputStreamStruct(javaString, start, end);
	}

	@Override
	protected String functionName() {
		return "MAKE-STRING-INPUT-STREAM";
	}
}
