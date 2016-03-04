/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams.functions;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.compiler.environment.binding.lambdalist.KeyParameter;
import jcl.compiler.environment.binding.lambdalist.OptionalParameter;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.printer.Printer;
import jcl.streams.OutputStream;
import jcl.streams.StreamVariables;
import jcl.symbols.KeywordStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.TStruct;
import jcl.system.CommonLispSymbols;
import jcl.types.BooleanType;
import jcl.types.IntegerType;
import jcl.types.NILType;
import jcl.types.StreamType;
import jcl.types.StringType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class WriteStringFunction extends AbstractCommonLispFunctionStruct {

	@Autowired
	private TypeValidator validator;

	@Autowired
	private Printer printer;

	public WriteStringFunction() {
		super("Writes the characters of the sub-sequence of string bounded by start and end to output-stream.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "STRING").buildList();
	}

	@Override
	protected List<OptionalParameter> getOptionalBindings() {
		return OptionalParameter.builder(GlobalPackageStruct.COMMON_LISP, "OUTPUT-STREAM")
		                        .suppliedPBinding()
		                        .initForm(StreamVariables.STANDARD_OUTPUT.getVariableValue())
		                        .buildList();
	}

	/**
	 * {@inheritDoc}
	 * Create the list of {@link KeyParameter}s for the bounding of the string to output.
	 *
	 * @return a list of {@link KeyParameter}s
	 */
	@Override
	protected List<KeyParameter> getKeyBindings() {
		final List<KeyParameter> keyParameters = new ArrayList<>(2);
		final KeyParameter startParam
				= KeyParameter.builder(GlobalPackageStruct.COMMON_LISP, "START")
				              .initForm(IntegerStruct.ZERO)
				              .suppliedPBinding()
				              .build();
		keyParameters.add(startParam);
		final KeyParameter endParam
				= KeyParameter.builder(GlobalPackageStruct.COMMON_LISP, "END")
				              .initForm(NILStruct.INSTANCE)
				              .suppliedPBinding()
				              .build();
		keyParameters.add(endParam);
		return keyParameters;
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final StringStruct stringParam
				= validator.validateType(lispStructs[0], functionName(), "String", StringType.INSTANCE, StringStruct.class);

		int keywordStart = 1;
		final int length = lispStructs.length;

		OutputStream outputStream = StreamVariables.STANDARD_OUTPUT.getVariableValue();
		if (length > 1) {
			final LispStruct lispStruct = lispStructs[1];
			if (!CommonLispSymbols.START_KEYWORD.equals(lispStruct) && !CommonLispSymbols.END_KEYWORD.equals(lispStruct)) {
				keywordStart++;

				validator.validateTypes(lispStruct, functionName(), "Output Stream", BooleanType.INSTANCE, StreamType.INSTANCE);
				if (TStruct.INSTANCE.equals(lispStruct)) {
					outputStream = StreamVariables.STANDARD_OUTPUT.getVariableValue();
				} else if (NILStruct.INSTANCE.equals(lispStruct)) {
					outputStream = StreamVariables.STANDARD_OUTPUT.getVariableValue();
				} else if (lispStruct instanceof OutputStream) {
					outputStream = (OutputStream) lispStruct;
				} else {
					final String printedObject = printer.print(lispStruct);
					throw new TypeErrorException("The value " + printedObject + " is not either T, NIL, or an Output Stream.");
				}
			}
		}

		final Map<KeywordStruct, LispStruct> keywords
				= getKeywords(lispStructs, keywordStart, CommonLispSymbols.START_KEYWORD, CommonLispSymbols.END_KEYWORD);

		final LispStruct startParam
				= keywords.getOrDefault(CommonLispSymbols.START_KEYWORD, IntegerStruct.ZERO);
		validator.validateTypes(startParam, functionName(), "Start", IntegerType.INSTANCE);
		final int start = ((IntegerStruct) startParam).getBigInteger().intValue();

		final LispStruct endParam
				= keywords.getOrDefault(CommonLispSymbols.END_KEYWORD, IntegerStruct.ZERO);
		validator.validateTypes(startParam, functionName(), "End", IntegerType.INSTANCE, NILType.INSTANCE);

		final String javaString = stringParam.getAsJavaString();
		if (endParam instanceof IntegerStruct) {
			final int end = ((IntegerStruct) endParam).getBigInteger().intValue();
			outputStream.writeString(javaString, start, end);
		} else {
			outputStream.writeString(javaString, start);
		}

		return startParam;
	}

	@Override
	protected String functionName() {
		return "WRITE-STRING";
	}
}
