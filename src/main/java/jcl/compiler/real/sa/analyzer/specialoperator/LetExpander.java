package jcl.compiler.real.sa.analyzer.specialoperator;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.sa.FormAnalyzer;
import jcl.compiler.real.struct.specialoperator.ClosureCreationStruct;
import jcl.compiler.real.struct.specialoperator.LetStruct;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class LetExpander extends ClosureCreationExpander<LetStruct.LetVar> {

	private static final long serialVersionUID = 2933802423859476026L;

	@Autowired
	private FormAnalyzer formAnalyzer;

	protected LetExpander() {
		super("LET");
	}

	@Override
	public SymbolStruct<?> getFunctionSymbol() {
		return SpecialOperatorStruct.LET;
	}

	@Override
	protected LetStruct.LetVar getClosureCreationVar(final SymbolStruct<?> var, final LispStruct initForm,
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
	protected LispStruct getListParameterInitForm(final ListStruct listParameter, final Environment environment) {
		final ListStruct listParameterRest = listParameter.getRest();
		final LispStruct parameterValue = listParameterRest.getFirst();

		// Evaluate in the outer environment. This is because we want to ensure we don't have references to symbols that may not exist.
		final Environment parentEnvironment = environment.getParent();
		return formAnalyzer.analyze(parameterValue, parentEnvironment);
	}
}
