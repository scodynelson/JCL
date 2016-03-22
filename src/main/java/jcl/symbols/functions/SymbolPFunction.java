package jcl.symbols.functions;

import jcl.LispStruct;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.symbols.BooleanStructs;
import jcl.symbols.SymbolStruct;
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
