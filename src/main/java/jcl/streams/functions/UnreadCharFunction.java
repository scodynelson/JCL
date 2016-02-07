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
import jcl.streams.InputStream;
import jcl.streams.StreamVariables;
import jcl.types.CharacterType;
import jcl.types.StreamType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class UnreadCharFunction extends AbstractCommonLispFunctionStruct {

	@Autowired
	private TypeValidator validator;

	@Autowired
	private Printer printer;

	public UnreadCharFunction() {
		super("Places character back onto the front of input-stream so that it will again be the next character in input-stream.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "CHARACTER").buildList();
	}

	@Override
	protected List<OptionalParameter> getOptionalBindings() {
		return OptionalParameter.builder(GlobalPackageStruct.COMMON_LISP, "INPUT-STREAM")
		                        .suppliedPBinding()
		                        .initForm(StreamVariables.STANDARD_INPUT.getVariableValue())
		                        .buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct lispStruct1 = lispStructs[0];
		validator.validateTypes(lispStruct1, functionName(), "Character", CharacterType.INSTANCE);
		final CharacterStruct character = (CharacterStruct) lispStruct1;

		InputStream inputStream = StreamVariables.STANDARD_INPUT.getVariableValue();
		if (lispStructs.length > 1) {
			final LispStruct lispStruct2 = lispStructs[1];
			validator.validateTypes(lispStruct2, functionName(), "Input Stream", StreamType.INSTANCE);

			if (!(lispStruct2 instanceof InputStream)) {
				final String printedObject = printer.print(lispStruct2);
				throw new TypeErrorException(functionName() + ": Input Stream must be an actual INPUT-STREAM. Got: " + printedObject);
			}

			inputStream = (InputStream) lispStruct2;
		}

		inputStream.unreadChar(character.getCodePoint());
		return character;
	}

	@Override
	protected String functionName() {
		return "UNREAD-CHAR";
	}
}
