package jcl.functions;

import jcl.compiler.function.InternalEval;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.statics.CommonLispSymbols;

public final class EvalFunction extends BuiltInFunctionStructImpl {

	private static final String FUNCTION_NAME = "EVAL";
	private static final String FORM_ARGUMENT = "FORM";

	public EvalFunction() {
		super("Evaluates form in the current dynamic environment and the null lexical environment.",
		      CommonLispSymbols.EVAL.getName(),
		      Parameters.forFunction(CommonLispSymbols.EVAL.getName())
		                .requiredParameter(FORM_ARGUMENT)
		);
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.EVAL;
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final LispStruct form = arguments.getRequiredArgument(FORM_ARGUMENT);
		return InternalEval.eval(form);
	}
}
