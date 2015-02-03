package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.element.ThrowElement;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import org.springframework.stereotype.Component;

@Component
public class ThrowAnalyzer implements SpecialOperatorAnalyzer {

	private static final long serialVersionUID = 359191567361134081L;

	@Override
	public ThrowElement analyze(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder) {

		if (input.size() != 3) {
			throw new ProgramErrorException("THROW: Incorrect number of arguments: " + input.size() + ". Expected 3 arguments.");
		}

		final LispStruct catchTag = input.getRest().getFirst();
		final LispStruct catchTagAnalyzed = analyzer.analyzeForm(catchTag, analysisBuilder);

		final LispStruct resultForm = input.getRest().getRest().getFirst();
		final LispStruct resultFormAnalyzed = analyzer.analyzeForm(resultForm, analysisBuilder);

		return new ThrowElement(catchTagAnalyzed, resultFormAnalyzed);
	}
}
