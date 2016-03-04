package jcl.lists.functions;

import jcl.LispType;
import jcl.functions.AbstractPredicateCommonLispFunction;
import jcl.types.ListType;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code listp}.
 */
@Component
public final class ListPFunction extends AbstractPredicateCommonLispFunction {

	/**
	 * Public constructor passing the documentation string.
	 */
	public ListPFunction() {
		super("Returns true if object is of type list; otherwise, returns false.");
	}

	/**
	 * {@inheritDoc}
	 * Returns the function name {@code listp} as a string.
	 *
	 * @return the function name {@code listp} as a string
	 */
	@Override
	protected String functionName() {
		return "LISTP";
	}

	/**
	 * {@inheritDoc}
	 * Returns {@link ListType#INSTANCE} as the type instance to check against.
	 *
	 * @return the {@link ListType#INSTANCE} singleton value
	 */
	@Override
	protected LispType testType() {
		return ListType.INSTANCE;
	}
}
