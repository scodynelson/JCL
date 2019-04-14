package jcl.functions.condition;

import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.LispStruct;
import jcl.lang.StringStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.statics.CommonLispSymbols;

public final class ErrorFunction extends BuiltInFunctionStructImpl {

	private static final String DATUM_ARGUMENT = "DATUM";

	public ErrorFunction() {
		super("Error effectively invokes signal on the denoted condition.",
		      CommonLispSymbols.ERROR.getName(),
		      Parameters.forFunction(CommonLispSymbols.ERROR.getName())
		                .requiredParameter(DATUM_ARGUMENT)
		                .restParameter()
		);
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.ERROR;
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final StringStruct datum = arguments.getRequiredArgument(DATUM_ARGUMENT, StringStruct.class);
		// TODO: Fix this when we actually create the condition system.
		throw new ErrorException(datum.toJavaString());
	}
}
