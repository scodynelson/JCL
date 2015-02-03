package jcl.compiler.real.sa.analyzer.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.element.CatchElement;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.stream.Collectors;

@Component
public class CatchAnalyzer implements SpecialOperatorAnalyzer {

	private static final long serialVersionUID = -4421664278117234704L;

	@Override
	public CatchElement analyze(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder) {

		final int inputSize = input.size();
		if (inputSize < 2) {
			throw new ProgramErrorException("CATCH: Incorrect number of arguments: " + inputSize + ". Expected at least 2 arguments.");
		}

		final LispStruct catchTag = input.getRest().getFirst();
		final LispStruct catchTagAnalyzed = analyzer.analyzeForm(catchTag, analysisBuilder);

		final ListStruct forms = input.getRest().getRest();
		final List<LispStruct> formsJavaList = forms.getAsJavaList();
		final List<LispStruct> analyzedForms =
				formsJavaList.stream()
				             .map(e -> analyzer.analyzeForm(e, analysisBuilder))
				             .collect(Collectors.toList());

		return new CatchElement(catchTagAnalyzed, analyzedForms);
	}
}
