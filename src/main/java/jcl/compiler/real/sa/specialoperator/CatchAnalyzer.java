package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.element.CatchElement;
import jcl.compiler.real.sa.specialoperator.body.BodyAnalyzer;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
public class CatchAnalyzer implements SpecialOperatorAnalyzer {

	@Autowired
	private BodyAnalyzer bodyAnalyzer;

	@Override
	public LispStruct analyze(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder) {

		if (input.size() < 2) {
			throw new ProgramErrorException("CATCH: Incorrect number of arguments: " + input.size() + ". Expected at least 2 arguments.");
		}

		final LispStruct catchTag = input.getRest().getFirst();
		final LispStruct catchTagAnalyzed = analyzer.analyzeForm(catchTag, analysisBuilder);

		final ListStruct forms = input.getRest().getRest();
		final List<LispStruct> formsAnalyzed = bodyAnalyzer.analyze(analyzer, forms, analysisBuilder);

		return new CatchElement(catchTagAnalyzed, formsAnalyzed);
	}
}
