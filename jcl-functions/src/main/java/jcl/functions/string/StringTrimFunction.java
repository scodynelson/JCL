package jcl.functions.string;

import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.LispStruct;
import jcl.lang.SequenceStruct;
import jcl.lang.StringStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class StringTrimFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "STRING-TRIM";
	private static final String CHARACTER_BAG_ARGUMENT = "CHARACTER-BAG";
	private static final String STRING_ARGUMENT = "STRING";

	public StringTrimFunction() {
		super("Returns a substring of string, with all characters in character-bag stripped off the beginning and end.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(CHARACTER_BAG_ARGUMENT)
		                .requiredParameter(STRING_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final SequenceStruct characterBag = arguments.getRequiredArgument(CHARACTER_BAG_ARGUMENT, SequenceStruct.class);
		final StringStruct struct = arguments.getRequiredArgument(STRING_ARGUMENT, StringStruct.class);
		return struct.stringTrim(characterBag);
	}
}
