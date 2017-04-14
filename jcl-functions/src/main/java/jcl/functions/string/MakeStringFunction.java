package jcl.functions.string;

import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.CharacterStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.StringStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.statics.CharacterConstants;
import jcl.lang.statics.CommonLispSymbols;
import jcl.type.CharacterType;
import org.springframework.stereotype.Component;

@Component
public final class MakeStringFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "MAKE-STRING";
	private static final String SIZE_ARGUMENT = "SIZE";

	public MakeStringFunction() {
		super("Returns a string just like string with all lowercase characters replaced by the corresponding uppercase characters.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(SIZE_ARGUMENT)
		                .keyParameter(CommonLispSymbols.INITIAL_ELEMENT_KEYWORD)
		                .withInitialValue(CharacterConstants.NULL_CHAR)
		                .keyParameter(CommonLispSymbols.ELEMENT_TYPE_KEYWORD)
		                .withInitialValue(CharacterType.INSTANCE)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final IntegerStruct size = arguments.getRequiredArgument(SIZE_ARGUMENT, IntegerStruct.class);

		final StringStruct.Builder builder = StringStruct.builder(size);
		if (arguments.hasKeyArgument(CommonLispSymbols.INITIAL_ELEMENT_KEYWORD)) {
			final CharacterStruct initialElement = arguments.getKeyArgument(CommonLispSymbols.INITIAL_ELEMENT_KEYWORD,
			                                                                CharacterStruct.class);
			builder.initialElement(initialElement);
		}
		if (arguments.hasKeyArgument(CommonLispSymbols.ELEMENT_TYPE_KEYWORD)) {
			final CharacterType elementType = arguments.getKeyArgument(CommonLispSymbols.ELEMENT_TYPE_KEYWORD,
			                                                           CharacterType.class);
			builder.elementType(elementType);
		}
		return builder.build();
	}
}
