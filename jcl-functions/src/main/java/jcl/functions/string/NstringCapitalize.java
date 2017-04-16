package jcl.functions.string;

import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.StringIntervalOpContext;
import jcl.lang.StringStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.statics.CommonLispSymbols;
import org.springframework.stereotype.Component;

@Component
public final class NstringCapitalize extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "NSTRING-CAPITALIZE";
	private static final String STRING_ARGUMENT = "STRING";

	public NstringCapitalize() {
		super("Modifies the provided string such that, for every word in the copy, the first character of the ``word,'' if it has case, is uppercase and any other characters with case in the word are lowercase.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(STRING_ARGUMENT)
		                .keyParameter(CommonLispSymbols.START_KEYWORD).withInitialValue(IntegerStruct.ZERO)
		                .keyParameter(CommonLispSymbols.END_KEYWORD).withInitialValue(NILStruct.INSTANCE)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final StringStruct struct = arguments.getRequiredArgument(STRING_ARGUMENT, StringStruct.class);

		final StringIntervalOpContext.Builder builder = StringIntervalOpContext.builder();
		if (arguments.hasKeyArgument(CommonLispSymbols.START_KEYWORD)) {
			final IntegerStruct start = arguments.getKeyArgument(CommonLispSymbols.START_KEYWORD, IntegerStruct.class);
			builder.start(start);
		}
		if (arguments.hasKeyArgument(CommonLispSymbols.END_KEYWORD)) {
			final IntegerStruct end = arguments.getKeyArgument(CommonLispSymbols.END_KEYWORD, IntegerStruct.class);
			builder.end(end);
		}
		return struct.nStringCapitalize(builder.build());
	}
}
