package jcl.functions.readtable;

import jcl.functions.SystemBuiltInFunctionStructBase;
import jcl.lang.LispStruct;
import jcl.lang.ReadtableStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.readtable.ReadtableCase;
import jcl.lang.statics.CommonLispSymbols;
import org.springframework.stereotype.Component;

@Component
public final class SetReadtableCaseFunction extends SystemBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "SET-READTABLE-CASE";
	private static final String READTABLE_ARGUMENT = "READTABLE";
	private static final String MODE_ARGUMENT = "MODE";

	public SetReadtableCaseFunction() {
		super("Sets the readtable case of the provided readtable to the provided mode value.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(READTABLE_ARGUMENT)
		                .requiredParameter(MODE_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final ReadtableStruct readtable = arguments.getRequiredArgument(READTABLE_ARGUMENT, ReadtableStruct.class);
		final SymbolStruct mode = arguments.getRequiredArgument(MODE_ARGUMENT, SymbolStruct.class);

		if (CommonLispSymbols.UPCASE_KEYWORD.eq(mode)) {
			readtable.setReadtableCase(ReadtableCase.UPCASE);
		} else if (CommonLispSymbols.DOWNCASE_KEYWORD.eq(mode)) {
			readtable.setReadtableCase(ReadtableCase.DOWNCASE);
		} else if (CommonLispSymbols.PRESERVE_KEYWORD.eq(mode)) {
			readtable.setReadtableCase(ReadtableCase.PRESERVE);
		} else if (CommonLispSymbols.INVERT_KEYWORD.eq(mode)) {
			readtable.setReadtableCase(ReadtableCase.INVERT);
		}
		return mode;
	}
}
