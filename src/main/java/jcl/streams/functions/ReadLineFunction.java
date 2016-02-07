/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams.functions;

import java.util.ArrayList;
import java.util.List;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.compiler.environment.binding.lambdalist.OptionalParameter;
import jcl.compiler.struct.ValuesStruct;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.lists.NullStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.printer.Printer;
import jcl.streams.InputStream;
import jcl.streams.ReadLineResult;
import jcl.streams.StreamVariables;
import jcl.symbols.BooleanStruct;
import jcl.symbols.BooleanStructs;
import jcl.symbols.NILStruct;
import jcl.symbols.TStruct;
import jcl.types.BooleanType;
import jcl.types.StreamType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class ReadLineFunction extends AbstractCommonLispFunctionStruct {

	@Autowired
	private TypeValidator validator;

	@Autowired
	private Printer printer;

	public ReadLineFunction() {
		super("Returns the next line of text that is terminated by a newline or end of file.");
	}

	@Override
	protected List<OptionalParameter> getOptionalBindings() {
		final List<OptionalParameter> optionalParameters = new ArrayList<>(4);

		final OptionalParameter inputStreamOptionalBinding =
				OptionalParameter.builder(GlobalPackageStruct.COMMON_LISP, "INPUT-STREAM")
				                 .suppliedPBinding()
				                 .initForm(StreamVariables.STANDARD_INPUT.getVariableValue())
				                 .build();
		optionalParameters.add(inputStreamOptionalBinding);
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
		final OptionalParameter recursivePOptionalBinding =
				OptionalParameter.builder(GlobalPackageStruct.COMMON_LISP, "RECURSIVE-P")
				                 .suppliedPBinding()
				                 .initForm(NILStruct.INSTANCE)
				                 .build();
		optionalParameters.add(recursivePOptionalBinding);

		return optionalParameters;
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final int length = lispStructs.length;

		InputStream inputStream = StreamVariables.STANDARD_INPUT.getVariableValue();
		if (length > 0) {
			final LispStruct lispStruct = lispStructs[0];
			validator.validateTypes(lispStruct, functionName(), "Input Stream", BooleanType.INSTANCE, StreamType.INSTANCE);
			if (TStruct.INSTANCE.equals(lispStruct)) {
				inputStream = StreamVariables.STANDARD_INPUT.getVariableValue();
			} else if (NILStruct.INSTANCE.equals(lispStruct) || NullStruct.INSTANCE.equals(lispStruct)) {
				inputStream = StreamVariables.STANDARD_INPUT.getVariableValue();
			} else if (lispStruct instanceof InputStream) {
				inputStream = (InputStream) lispStruct;
			} else {
				final String printedObject = printer.print(lispStruct);
				throw new TypeErrorException("The value " + printedObject + " is not either T, NIL, or an Input Stream.");
			}
		}

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

		BooleanStruct recursiveP = NILStruct.INSTANCE;
		if (length > 3) {
			final LispStruct lispStruct = lispStructs[3];
			validator.validateTypes(lispStruct, functionName(), "Recursive Predicate", BooleanType.INSTANCE);
			recursiveP = (BooleanStruct) lispStruct;
		}

		final ReadLineResult readLineResult = inputStream.readLine(eofErrorP.booleanValue(), eofValue, recursiveP.booleanValue());
		final String result = readLineResult.getResult();
		final boolean eof = readLineResult.isEof();
		return new ValuesStruct(new StringStruct(result), BooleanStructs.toLispBoolean(eof));
	}

	@Override
	protected String functionName() {
		return "READ-LINE";
	}
}
