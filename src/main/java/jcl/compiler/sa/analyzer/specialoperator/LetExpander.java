package jcl.compiler.sa.analyzer.specialoperator;

import java.util.List;

import jcl.compiler.environment.Environment;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.struct.specialoperator.BindingEnvironmentStruct;
import jcl.compiler.struct.specialoperator.LetStruct;
import jcl.compiler.struct.specialoperator.PrognStruct;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.statics.CommonLispSymbols;

public final class LetExpander extends BindingEnvironmentExpander {

	public static final LetExpander INSTANCE = new LetExpander();

	private LetExpander() {
		super("LET");
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.LET;
	}

	@Override
	protected BindingEnvironmentStruct getBindingEnvironmentStruct(final List<BindingEnvironmentStruct.BindingVar> vars,
	                                                               final PrognStruct prognBody,
	                                                               final Environment environment) {
		return new LetStruct(vars, prognBody, environment);
	}

	@Override
	protected LispStruct getListParameterInitForm(final LispStruct parameterValue, final Environment environment) {
		// Evaluate in the outer environment. This is because we want to ensure we don't have references to symbols that may not exist.
		final Environment parentEnvironment = environment.getParent();
		return FormAnalyzer.analyze(parameterValue, parentEnvironment);
	}
}
