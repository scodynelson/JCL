package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.element.IfElement;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import org.springframework.stereotype.Component;

@Component
public class IfAnalyzer implements SpecialOperatorAnalyzer {

	private static final long serialVersionUID = -5414856145190749144L;

	@Override
	public IfElement analyze(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder) {

		if ((input.size() < 3) || (input.size() > 4)) {
			throw new ProgramErrorException("IF: Incorrect number of arguments: " + input.size() + ". Expected either 3 or 4 arguments.");
		}

		final LispStruct testForm = input.getRest().getFirst();
		final LispStruct testFormAnalyzed = analyzer.analyzeForm(testForm, analysisBuilder);

		final LispStruct thenForm = input.getRest().getRest().getFirst();
		final LispStruct thenFormAnalyzed = analyzer.analyzeForm(thenForm, analysisBuilder);

		if (input.size() == 4) {
			final LispStruct elseForm = input.getRest().getRest().getRest().getFirst();
			final LispStruct elseFormAnalyzed = analyzer.analyzeForm(elseForm, analysisBuilder);
			return new IfElement(testFormAnalyzed, thenFormAnalyzed, elseFormAnalyzed);
		} else {
			return new IfElement(testFormAnalyzed, thenFormAnalyzed);
		}
	}
}
