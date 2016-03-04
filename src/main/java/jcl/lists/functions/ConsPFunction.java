package jcl.lists.functions;

import jcl.LispType;
import jcl.functions.AbstractPredicateCommonLispFunction;
import jcl.types.ConsType;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code consp}.
 */
@Component
public final class ConsPFunction extends AbstractPredicateCommonLispFunction {

	/**
	 * Public constructor passing the documentation string.
	 */
	public ConsPFunction() {
		super("Returns true if object is of type cons; otherwise, returns false.");
	}

	/**
	 * {@inheritDoc}
	 * Returns the function name {@code consp} as a string.
	 *
	 * @return the function name {@code consp} as a string
	 */
	@Override
	protected String functionName() {
		return "CONSP";
	}

	/**
	 * {@inheritDoc}
	 * Returns {@link ConsType#INSTANCE} as the type instance to check against.
	 *
	 * @return the {@link ConsType#INSTANCE} singleton value
	 */
	@Override
	protected LispType testType() {
		return ConsType.INSTANCE;
	}
}
