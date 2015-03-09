package jcl.compiler.real.sa.analyzer.specialoperator;

import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.analyzer.expander.real.MacroFunctionExpander;
import jcl.compiler.real.struct.specialoperator.IfStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;
import org.springframework.stereotype.Component;

@Component
public class IfAnalyzer extends MacroFunctionExpander implements SpecialOperatorAnalyzer {

	private static final long serialVersionUID = -5414856145190749144L;

	/**
	 * Initializes the block macro function and adds it to the special operator 'block'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperator.IF.setMacroFunctionExpander(this);
	}

	@Override
	public LispStruct expand(final ListStruct form, final AnalysisBuilder analysisBuilder) {
		return analyze(form, analysisBuilder);
	}

	@Override
	public IfStruct analyze(final ListStruct input, final AnalysisBuilder analysisBuilder) {

		final int inputSize = input.size();
		if ((inputSize < 3) || (inputSize > 4)) {
			throw new ProgramErrorException("IF: Incorrect number of arguments: " + inputSize + ". Expected either 3 or 4 arguments.");
		}

		final ListStruct inputRest = input.getRest();

		final SemanticAnalyzer analyzer = analysisBuilder.getAnalyzer();

		final LispStruct testForm = inputRest.getFirst();
		final LispStruct testFormAnalyzed = analyzer.analyzeForm(testForm, analysisBuilder);

		final ListStruct inputRestRest = inputRest.getRest();

		final LispStruct thenForm = inputRestRest.getFirst();
		final LispStruct thenFormAnalyzed = analyzer.analyzeForm(thenForm, analysisBuilder);

		if (inputSize == 4) {
			final ListStruct inputRestRestRest = inputRestRest.getRest();

			final LispStruct elseForm = inputRestRestRest.getFirst();
			final LispStruct elseFormAnalyzed = analyzer.analyzeForm(elseForm, analysisBuilder);
			return new IfStruct(testFormAnalyzed, thenFormAnalyzed, elseFormAnalyzed);
		} else {
			return new IfStruct(testFormAnalyzed, thenFormAnalyzed);
		}
	}
}
