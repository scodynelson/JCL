package jcl.compiler.real.sa.analyzer.specialoperator;

import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.analyzer.expander.real.MacroFunctionExpander;
import jcl.compiler.real.struct.specialoperator.ThrowStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;
import org.springframework.stereotype.Component;

@Component
public class ThrowAnalyzer extends MacroFunctionExpander implements SpecialOperatorAnalyzer {

	private static final long serialVersionUID = 359191567361134081L;

	/**
	 * Initializes the block macro function and adds it to the special operator 'block'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperator.THROW.setMacroFunctionExpander(this);
	}

	@Override
	public LispStruct expand(final ListStruct form, final AnalysisBuilder analysisBuilder) {
		return analyze(form, analysisBuilder);
	}

	@Override
	public ThrowStruct analyze(final ListStruct input, final AnalysisBuilder analysisBuilder) {

		final int inputSize = input.size();
		if (inputSize != 3) {
			throw new ProgramErrorException("THROW: Incorrect number of arguments: " + inputSize + ". Expected 3 arguments.");
		}

		final ListStruct inputRest = input.getRest();

		final SemanticAnalyzer analyzer = analysisBuilder.getAnalyzer();

		final LispStruct catchTag = inputRest.getFirst();
		final LispStruct catchTagAnalyzed = analyzer.analyzeForm(catchTag, analysisBuilder);

		final ListStruct inputRestRest = inputRest.getRest();

		final LispStruct resultForm = inputRestRest.getFirst();
		final LispStruct resultFormAnalyzed = analyzer.analyzeForm(resultForm, analysisBuilder);

		return new ThrowStruct(catchTagAnalyzed, resultFormAnalyzed);
	}
}
