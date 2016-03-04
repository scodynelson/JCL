/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams.functions;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.OptionalParameter;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.printer.Printer;
import jcl.streams.OutputStream;
import jcl.streams.StreamVariables;
import jcl.symbols.BooleanStructs;
import jcl.types.StreamType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class FreshLineFunction extends AbstractCommonLispFunctionStruct {

	@Autowired
	private TypeValidator validator;

	@Autowired
	private Printer printer;

	public FreshLineFunction() {
		super("Outputs a newline only if the output-stream is not already at the start of a line.");
	}

	@Override
	protected List<OptionalParameter> getOptionalBindings() {
		return OptionalParameter.builder(GlobalPackageStruct.COMMON_LISP, "OUTPUT-STREAM")
		                        .suppliedPBinding()
		                        .initForm(StreamVariables.STANDARD_OUTPUT.getVariableValue())
		                        .buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		OutputStream outputStream = StreamVariables.STANDARD_OUTPUT.getVariableValue();
		if (lispStructs.length > 0) {
			final LispStruct lispStruct = lispStructs[0];
			validator.validateTypes(lispStruct, functionName(), "Output Stream", StreamType.INSTANCE);

			if (!(lispStruct instanceof OutputStream)) {
				final String printedObject = printer.print(lispStruct);
				throw new TypeErrorException(functionName() + ": Output Stream must be an actual OUTPUT-STREAM. Got: " + printedObject);
			}

			outputStream = (OutputStream) lispStruct;
		}

		final boolean shouldWriteNewline = !outputStream.isStartOfLine();
		if (shouldWriteNewline) {
			outputStream.writeChar('\n');
		}

		return BooleanStructs.toLispBoolean(shouldWriteNewline);
	}

	@Override
	protected String functionName() {
		return "FRESHLINE";
	}
}