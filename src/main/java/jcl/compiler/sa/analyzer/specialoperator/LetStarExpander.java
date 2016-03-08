package jcl.compiler.sa.analyzer.specialoperator;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.Environment;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.struct.specialoperator.ClosureCreationStruct;
import jcl.compiler.struct.specialoperator.LetStarStruct;
import jcl.compiler.struct.specialoperator.PrognStruct;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class LetStarExpander extends ClosureCreationExpander<LetStarStruct.LetStarVar> {

	@Autowired
	private FormAnalyzer formAnalyzer;

	protected LetStarExpander() {
		super("LET*");
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return SpecialOperatorStruct.LET_STAR;
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
	protected LispStruct getListParameterInitForm(final ListStruct listParameter, final Environment environment) {
		final ListStruct listParameterRest = listParameter.getRest();
		final LispStruct parameterValue = listParameterRest.getCar();

		// Evaluate in the 'current' environment.
		return formAnalyzer.analyze(parameterValue, environment);
	}
}
