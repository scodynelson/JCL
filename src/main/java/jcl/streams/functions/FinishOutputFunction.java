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
import jcl.symbols.NILStruct;
import jcl.types.StreamType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class FinishOutputFunction extends AbstractCommonLispFunctionStruct {

	@Autowired
	private TypeValidator validator;

	@Autowired
	private Printer printer;

	public FinishOutputFunction() {
		super("Attempts to ensure that any buffered output sent to output-stream has reached its destination, and then returns.");
	}

	@Override
	protected List<OptionalParameter> getOptionalBindings() {
		return OptionalParameter.builder(GlobalPackageStruct.COMMON_LISP, "OUTPUT-STREAM").buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		if (lispStructs.length == 0) {
			StreamVariables.STANDARD_OUTPUT.getVariableValue().finishOutput();
			return NILStruct.INSTANCE;
		}

		final LispStruct lispStruct = lispStructs[0];
		validator.validateTypes(lispStruct, functionName(), "Output Stream", StreamType.INSTANCE);

		if (!(lispStruct instanceof OutputStream)) {
			final String printedObject = printer.print(lispStruct);
			throw new TypeErrorException(functionName() + ": Output Stream must be an actual OUTPUT-STREAM. Got: " + printedObject);
		}

		final OutputStream outputStream = (OutputStream) lispStruct;
		outputStream.finishOutput();
		return NILStruct.INSTANCE;
	}

	@Override
	protected String functionName() {
		return "FINISH-OUTPUT";
	}
}
