package jcl.functions.string;

import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.BooleanStruct;
import jcl.lang.LispStruct;
import jcl.lang.StringStruct;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class SimpleStringPFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "STRING-STRING-P";
	private static final String OBJECT_ARGUMENT = "OBJECT";

	public SimpleStringPFunction() {
		super("Returns true if object is of type simple-string; otherwise, returns false.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(OBJECT_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final LispStruct object = arguments.getRequiredArgument(OBJECT_ARGUMENT);
		final boolean isString = object instanceof StringStruct;
		if (!isString) {
			return BooleanStruct.NIL;
		}
		return LispStructFactory.toBoolean(((StringStruct) object).isSimpleString());
	}
}
