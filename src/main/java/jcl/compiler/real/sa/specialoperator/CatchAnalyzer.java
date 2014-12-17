package jcl.compiler.real.sa.specialoperator;

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

	@Override
	public LispStruct analyze(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder) {

		if (input.size() < 2) {
			throw new ProgramErrorException("CATCH: Incorrect number of arguments: " + input.size() + ". Expected at least 2 arguments.");
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
