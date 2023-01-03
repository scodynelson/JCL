package jcl.compiler.function;

import jcl.compiler.environment.Environment;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.struct.CompilerSpecialOperatorStruct;
import jcl.compiler.struct.specialoperator.CompilerFunctionStruct;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import lombok.experimental.UtilityClass;

@UtilityClass
public final class InternalEval {

	public static LispStruct eval(final LispStruct originalExp) {

		final Environment nullEnvironment = Environment.NULL;
		return eval(originalExp, nullEnvironment);
	}

	public static LispStruct eval(final LispStruct originalExp, final Environment environment) {

		final LispStruct exp = FormAnalyzer.analyze(originalExp, environment);

		if (exp instanceof SymbolStruct) {
			return environment.getSymbolValue((SymbolStruct) exp);
		}
		if (exp instanceof CompilerFunctionStruct) {
			return ((CompilerFunctionStruct) exp).eval(environment);
		}
		if (exp instanceof CompilerSpecialOperatorStruct) {
			return ((CompilerSpecialOperatorStruct) exp).eval(environment);
		}

		return exp;
	}
}
