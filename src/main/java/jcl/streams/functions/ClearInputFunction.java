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
import jcl.streams.InputStream;
import jcl.streams.StreamVariables;
import jcl.symbols.NILStruct;
import jcl.types.StreamType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class ClearInputFunction extends AbstractCommonLispFunctionStruct {

	@Autowired
	private TypeValidator validator;

	@Autowired
	private Printer printer;

	public ClearInputFunction() {
		super("Clears any available input from input-stream.");
	}

	@Override
	protected List<OptionalParameter> getOptionalBindings() {
		return OptionalParameter.builder(GlobalPackageStruct.COMMON_LISP, "INPUT-STREAM").buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		if (lispStructs.length == 0) {
			StreamVariables.STANDARD_INPUT.getVariableValue().clearInput();
			return NILStruct.INSTANCE;
		}

		final LispStruct lispStruct = lispStructs[0];
		validator.validateTypes(lispStruct, functionName(), "Input Stream", StreamType.INSTANCE);

		if (!(lispStruct instanceof InputStream)) {
			final String printedObject = printer.print(lispStruct);
			throw new TypeErrorException(functionName() + ": Input Stream must be an actual INPUT-STREAM. Got: " + printedObject);
		}

		final InputStream inputStream = (InputStream) lispStruct;
		inputStream.clearInput();
		return NILStruct.INSTANCE;
	}

	@Override
	protected String functionName() {
		return "CLEAR-INPUT";
	}
}
