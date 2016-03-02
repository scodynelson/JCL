package jcl.symbols.functions;

import jcl.LispType;
import jcl.functions.AbstractPredicateCommonLispFunction;
import jcl.types.SymbolType;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code symbolp}.
 */
@Component
public final class SymbolPFunction extends AbstractPredicateCommonLispFunction {

	/**
	 * Public constructor passing the documentation string.
	 */
	public SymbolPFunction() {
		super("Returns true if object is of type symbol; otherwise, returns false.");
	}

	/**
	 * {@inheritDoc}
	 * Returns the function name {@code symbolp} as a string.
	 *
	 * @return the function name {@code symbolp} as a string
	 */
	@Override
	protected String functionName() {
		return "SYMBOLP";
	}

	/**
	 * {@inheritDoc}
	 * Returns {@link SymbolType#INSTANCE} as the type instance to check against.
	 *
	 * @return the {@link SymbolType#INSTANCE} singleton value
	 */
	@Override
	protected LispType testType() {
		return SymbolType.INSTANCE;
	}
}
