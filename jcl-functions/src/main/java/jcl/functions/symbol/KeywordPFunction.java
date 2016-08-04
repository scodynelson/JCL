package jcl.functions.symbol;

import jcl.lang.KeywordStruct;
import jcl.lang.LispStruct;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code keywordp}.
 */
@Component
public final class KeywordPFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "KEYWORDP";
	private static final String OBJECT_ARGUMENT = "OBJECT";

	/**
	 * Public constructor passing the documentation string.
	 */
	public KeywordPFunction() {
		super("Returns true if object is of type keyword; otherwise, returns false.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(OBJECT_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final LispStruct object = arguments.getRequiredArgument(OBJECT_ARGUMENT);
		return LispStructFactory.toBoolean(object instanceof KeywordStruct);
	}
}
