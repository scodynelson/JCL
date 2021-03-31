package jcl.compiler.sa.analyzer.specialoperator;

import java.util.List;

import jcl.compiler.environment.Environment;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.struct.specialoperator.BindingEnvironmentStruct;
import jcl.compiler.struct.specialoperator.LetStarStruct;
import jcl.compiler.struct.specialoperator.PrognStruct;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.internal.SpecialOperatorStructImpl;

public final class LetStarExpander extends BindingEnvironmentExpander<LetStarStruct.LetStarVar> {

	public static final LetStarExpander INSTANCE = new LetStarExpander();

	private LetStarExpander() {
		super("LET*");
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return SpecialOperatorStructImpl.LET_STAR;
	}

	@Override
	protected LetStarStruct.LetStarVar getBindingEnvironmentVar(final SymbolStruct var, final LispStruct initForm,
	                                                            final boolean isSpecial) {
		return new LetStarStruct.LetStarVar(var, initForm, isSpecial);
	}

	@Override
	protected BindingEnvironmentStruct<LetStarStruct.LetStarVar> getBindingEnvironmentStruct(final List<LetStarStruct.LetStarVar> vars,
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
