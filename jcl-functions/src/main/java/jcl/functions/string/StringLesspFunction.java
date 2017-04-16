package jcl.functions.string;

import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.IntegerStruct;
import jcl.lang.KeywordStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.StringEqualityContext;
import jcl.lang.StringStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.internal.KeywordStructImpl;
import jcl.lang.statics.CommonLispSymbols;
import org.springframework.stereotype.Component;

@Component
public final class StringLesspFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "STRING-LESSP";
	private static final String STRING1_ARGUMENT = "STRING1";
	private static final String STRING2_ARGUMENT = "STRING2";
	private static final KeywordStruct START1_ARGUMENT = KeywordStructImpl.valueOf("start1");
	private static final KeywordStruct END1_ARGUMENT = KeywordStructImpl.valueOf("end1");
	private static final KeywordStruct START2_ARGUMENT = KeywordStructImpl.valueOf("start2");
	private static final KeywordStruct END2_ARGUMENT = KeywordStructImpl.valueOf("end2");

	public StringLesspFunction() {
		super("String-lessp is exactly like string<, except that distinctions between uppercase and lowercase letters are ignored. It is as if char-lessp were used instead of char< for comparing characters.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(STRING1_ARGUMENT)
		                .requiredParameter(STRING2_ARGUMENT)
		                .keyParameter(START1_ARGUMENT).withInitialValue(IntegerStruct.ZERO)
		                .keyParameter(END1_ARGUMENT).withInitialValue(NILStruct.INSTANCE)
		                .keyParameter(START2_ARGUMENT).withInitialValue(IntegerStruct.ZERO)
		                .keyParameter(END2_ARGUMENT).withInitialValue(NILStruct.INSTANCE)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final StringStruct struct1 = arguments.getRequiredArgument(STRING1_ARGUMENT, StringStruct.class);
		final StringStruct struct2 = arguments.getRequiredArgument(STRING2_ARGUMENT, StringStruct.class);

		final StringEqualityContext.Builder builder = StringEqualityContext.builder(struct2);
		if (arguments.hasKeyArgument(START1_ARGUMENT)) {
			final IntegerStruct start1 = arguments.getKeyArgument(CommonLispSymbols.START_KEYWORD, IntegerStruct.class);
			builder.start1(start1);
		}
		if (arguments.hasKeyArgument(END1_ARGUMENT)) {
			final IntegerStruct end1 = arguments.getKeyArgument(CommonLispSymbols.END_KEYWORD, IntegerStruct.class);
			builder.end1(end1);
		}
		if (arguments.hasKeyArgument(START2_ARGUMENT)) {
			final IntegerStruct start2 = arguments.getKeyArgument(CommonLispSymbols.START_KEYWORD, IntegerStruct.class);
			builder.start2(start2);
		}
		if (arguments.hasKeyArgument(END2_ARGUMENT)) {
			final IntegerStruct end2 = arguments.getKeyArgument(CommonLispSymbols.END_KEYWORD, IntegerStruct.class);
			builder.end2(end2);
		}
		return struct1.stringLessThanIgnoreCase(builder.build());
	}
}
