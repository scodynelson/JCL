/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams.functions;

import java.util.List;

import jcl.LispStruct;
import jcl.characters.CharacterStruct;
import jcl.compiler.environment.binding.lambdalist.OptionalParameter;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.printer.Printer;
import jcl.streams.OutputStream;
import jcl.streams.StreamVariables;
import jcl.types.CharacterType;
import jcl.types.StreamType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class WriteCharFunction extends AbstractCommonLispFunctionStruct {

	@Autowired
	private TypeValidator validator;

	@Autowired
	private Printer printer;

	public WriteCharFunction() {
		super("Outputs character to output-stream.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "CHARACTER").buildList();
	}

	@Override
	protected List<OptionalParameter> getOptionalBindings() {
		return OptionalParameter.builder(GlobalPackageStruct.COMMON_LISP, "OUTPUT-STREAM")
		                        .suppliedPBinding()
		                        .initForm(StreamVariables.STANDARD_OUTPUT)
		                        .buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct lispStruct1 = lispStructs[0];
		validator.validateTypes(lispStruct1, functionName(), "Character", CharacterType.INSTANCE);
		final CharacterStruct character = (CharacterStruct) lispStruct1;

		final OutputStream outputStream;
		if (lispStructs.length > 1) {
			final LispStruct lispStruct2 = lispStructs[1];
			validator.validateTypes(lispStruct2, functionName(), "Output Stream", StreamType.INSTANCE);

			if (!(lispStruct2 instanceof OutputStream)) {
				final String printedObject = printer.print(lispStruct2);
				throw new TypeErrorException(functionName() + ": Input Stream must be an actual OUTPUT-STREAM. Got: " + printedObject);
			}

			outputStream = (OutputStream) lispStruct2;
		} else {
			outputStream = StreamVariables.STANDARD_OUTPUT.getVariableValue();
		}

		outputStream.writeChar(character.getCodePoint());
		return character;
	}

	@Override
	protected String functionName() {
		return "WRITE-CHAR";
	}
}
