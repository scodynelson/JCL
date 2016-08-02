/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.character;

import jcl.lang.CharacterStruct;
import jcl.lang.LispStruct;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.list.NILStruct;
import jcl.lang.number.IntegerStructImpl;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code code-char}.
 */
@Component
public final class CodeCharFunction extends CommonLispBuiltInFunctionStruct {

	/**
	 * Public constructor passing the documentation string.
	 */
	public CodeCharFunction() {
		super("Returns a character with the code attribute given by code. If no such character exists and one cannot be " +
				      "created, nil is returned.",
		      "CODE-CHAR",
		      Parameters.forFunction("CODE-CHAR")
		                .requiredParameter("CODE")
		);
	}

	/**
	 * {@inheritDoc}
	 * Application method for the {@code code-char} character function that expects a single {@link IntegerStructImpl}
	 * parameter object and applies {@link CharacterStruct#codeChar(IntegerStructImpl)} against the value to retrieve the
	 * {@link CharacterStruct} for the {@link IntegerStructImpl} parameter code value.
	 *
	 * @param lispStructs
	 * 		the function parameters
	 *
	 * @return the {@link CharacterStruct} for the {@link IntegerStructImpl} parameter code value
	 */
	@Override
	public LispStruct apply(final Arguments arguments) {
		final IntegerStructImpl code = arguments.getRequiredArgument("CODE", IntegerStructImpl.class);
		final int codeValue = code.intValue();
		if (!Character.isDefined(codeValue)) {
			return NILStruct.INSTANCE;
		}
		return LispStructFactory.toCharacter(codeValue);
	}
}
