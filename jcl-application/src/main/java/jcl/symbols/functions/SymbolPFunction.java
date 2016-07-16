package jcl.symbols.functions;

import jcl.lang.BooleanStructs;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code symbolp}.
 */
@Component
public final class SymbolPFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "SYMBOLP";
	private static final String OBJECT_ARGUMENT = "OBJECT";

	/**
	 * Public constructor passing the documentation string.
	 */
	public SymbolPFunction() {
		super("Returns true if object is of type symbol; otherwise, returns false.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(OBJECT_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final LispStruct object = arguments.getRequiredArgument(OBJECT_ARGUMENT);
		return BooleanStructs.toLispBoolean(object instanceof SymbolStruct);
	}
}
