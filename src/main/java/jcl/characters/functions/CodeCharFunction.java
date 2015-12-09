/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.List;

import jcl.LispStruct;
import jcl.characters.CharacterStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.types.IntegerType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code code-char}.
 */
@Component
public final class CodeCharFunction extends AbstractCommonLispFunctionStruct {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -7853448883104247869L;

	/**
	 * The {@link TypeValidator} for validating the function parameter value types.
	 */
	@Autowired
	private TypeValidator validator;

	/**
	 * Public constructor passing the documentation string.
	 */
	public CodeCharFunction() {
		super("Returns a character with the code attribute given by code. If no such character exists and one cannot be " +
				"created, nil is returned.");
	}

	/**
	 * {@inheritDoc}
	 * Creates the single {@link RequiredParameter} integer object for this function.
	 *
	 * @return a list of a single {@link RequiredParameter} integer object
	 */
	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return new RequiredParameter.Builder(GlobalPackageStruct.COMMON_LISP, "CODE").buildList();
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
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		validator.validateTypes(lispStruct, functionName(), "Code", IntegerType.INSTANCE);

		final IntegerStruct code = (IntegerStruct) lispStruct;
		return CharacterStruct.codeChar(code);
	}

	/**
	 * {@inheritDoc}
	 * Returns the function name {@code code-char} as a string.
	 *
	 * @return the function name {@code code-char} as a string
	 */
	@Override
	protected String functionName() {
		return "CODE-CHAR";
	}
}
