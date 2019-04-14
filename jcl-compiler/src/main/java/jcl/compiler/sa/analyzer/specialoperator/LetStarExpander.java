package jcl.compiler.sa.analyzer.specialoperator;

import java.util.List;

import jcl.compiler.environment.Environment;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.struct.specialoperator.ClosureCreationStruct;
import jcl.compiler.struct.specialoperator.LetStarStruct;
import jcl.compiler.struct.specialoperator.PrognStruct;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.internal.SpecialOperatorStructImpl;

public final class LetStarExpander extends ClosureCreationExpander<LetStarStruct.LetStarVar> {

	public static final LetStarExpander INSTANCE = new LetStarExpander();

	private LetStarExpander() {
		super("LET*");
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return SpecialOperatorStructImpl.LET_STAR;
	}

	@Override
	protected LetStarStruct.LetStarVar getClosureCreationVar(final SymbolStruct var, final LispStruct initForm,
	                                                         final boolean isSpecial) {
		return new LetStarStruct.LetStarVar(var, initForm, isSpecial);
	}

	@Override
	protected ClosureCreationStruct<LetStarStruct.LetStarVar> getClosureCreationStruct(final List<LetStarStruct.LetStarVar> vars,
	                                                                                   final PrognStruct prognBody,
	                                                                                   final Environment environment) {
		return new LetStarStruct(vars, prognBody, environment);
	}

	@Override
	protected LispStruct getListParameterInitForm(final LispStruct parameterValue, final Environment environment) {
		// Evaluate in the 'current' environment.
		return FormAnalyzer.analyze(parameterValue, environment);
	}
}
