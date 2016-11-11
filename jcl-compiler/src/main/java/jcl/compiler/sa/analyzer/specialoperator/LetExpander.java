package jcl.compiler.sa.analyzer.specialoperator;

import java.util.List;

import jcl.compiler.environment.Environment;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.struct.specialoperator.ClosureCreationStruct;
import jcl.compiler.struct.specialoperator.LetStruct;
import jcl.compiler.struct.specialoperator.PrognStruct;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.internal.SpecialOperatorStructImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class LetExpander extends ClosureCreationExpander<LetStruct.LetVar> {

	@Autowired
	private FormAnalyzer formAnalyzer;

	protected LetExpander() {
		super("LET");
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return SpecialOperatorStructImpl.LET;
	}

	@Override
	protected LetStruct.LetVar getClosureCreationVar(final SymbolStruct var, final LispStruct initForm,
	                                                 final boolean isSpecial) {
		return new LetStruct.LetVar(var, initForm, isSpecial);
	}

	@Override
	protected ClosureCreationStruct<LetStruct.LetVar> getClosureCreationStruct(final List<LetStruct.LetVar> vars,
	                                                                           final PrognStruct prognBody,
	                                                                           final Environment environment) {
		return new LetStruct(vars, prognBody, environment);
	}

	@Override
	protected LispStruct getListParameterInitForm(final LispStruct parameterValue, final Environment environment) {
		// Evaluate in the outer environment. This is because we want to ensure we don't have references to symbols that may not exist.
		final Environment parentEnvironment = environment.getParent();
		return formAnalyzer.analyze(parameterValue, parentEnvironment);
	}
}
