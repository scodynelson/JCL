/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams.functions;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.conditions.ConditionType;
import jcl.conditions.exceptions.StreamErrorException;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.streams.StreamStruct;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code stream-error-stream}.
 */
@Component
public final class StreamErrorStreamFunction extends AbstractCommonLispFunctionStruct {

	/**
	 * The {@link TypeValidator} for validating the function parameter value types.
	 */
	@Autowired
	private TypeValidator validator;

	/**
	 * Public constructor passing the documentation string.
	 */
	public StreamErrorStreamFunction() {
		super("Returns the offending stream in the situation represented by the condition.");
	}

	/**
	 * {@inheritDoc}
	 * Creates the single {@link RequiredParameter} condition object for this function.
	 *
	 * @return a list of a single {@link RequiredParameter} condition object
	 */
	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "CONDITION").buildList();
	}

	/**
	 * {@inheritDoc}
	 * Application method for {@code stream-error-stream} package function that returns the {@link StreamStruct} that
	 * was a part of provided {@link StreamErrorException} condition.
	 *
	 * @param lispStructs
	 * 		the function parameters
	 *
	 * @return the {@link StreamStruct} that was a part of provided {@link StreamErrorException} condition
	 */
	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		validator.validateTypes(lispStruct, functionName(), "Stream Error", ConditionType.STREAM_ERROR);

		return ((StreamErrorException) lispStruct).getStreamWithError();
	}

	/**
	 * {@inheritDoc}
	 * Returns the function name {@code stream-error-stream} as a string.
	 *
	 * @return the function name {@code stream-error-stream} as a string
	 */
	@Override
	protected String functionName() {
		return "STREAM-ERROR-STREAM";
	}
}
