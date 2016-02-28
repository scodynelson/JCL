package jcl.symbols.functions;

import jcl.LispType;
import jcl.functions.AbstractPredicateCommonLispFunction;
import jcl.types.KeywordType;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code keywordp}.
 */
@Component
public final class KeywordPFunction extends AbstractPredicateCommonLispFunction {

	/**
	 * Public constructor passing the documentation string.
	 */
	public KeywordPFunction() {
		super("Returns true if object is of type keyword; otherwise, returns false.");
	}

	/**
	 * {@inheritDoc}
	 * Returns the function name {@code keywordp} as a string.
	 *
	 * @return the function name {@code keywordp} as a string
	 */
	@Override
	protected String functionName() {
		return "KEYWORDP";
	}

	/**
	 * {@inheritDoc}
	 * Returns {@link KeywordType#INSTANCE} as the type instance to check against.
	 *
	 * @return the {@link KeywordType#INSTANCE} singleton value
	 */
	@Override
	protected LispType testType() {
		return KeywordType.INSTANCE;
	}
}
