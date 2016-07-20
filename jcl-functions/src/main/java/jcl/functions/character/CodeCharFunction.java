/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.character;

import jcl.lang.character.CharacterStruct;
import jcl.lang.LispStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.number.IntegerStruct;
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
	 * Application method for the {@code code-char} character function that expects a single {@link IntegerStruct}
	 * parameter object and applies {@link CharacterStruct#codeChar(IntegerStruct)} against the value to retrieve the
	 * {@link CharacterStruct} for the {@link IntegerStruct} parameter code value.
	 *
	 * @param lispStructs
	 * 		the function parameters
	 *
	 * @return the {@link CharacterStruct} for the {@link IntegerStruct} parameter code value
	 */
	@Override
	public LispStruct apply(final Arguments arguments) {
		final IntegerStruct code = arguments.getRequiredArgument("CODE", IntegerStruct.class);
		return CharacterStruct.codeChar(code);
	}
}
