package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.element.MultipleValueCallElement;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.stream.Collectors;

@Component
public class MultipleValueCallAnalyzer implements SpecialOperatorAnalyzer {

	private static final long serialVersionUID = -8350781874747372684L;

	@Override
	public MultipleValueCallElement analyze(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder) {

		if (input.size() < 2) {
			throw new ProgramErrorException("MULTIPLE-VALUE-CALL: Incorrect number of arguments: " + input.size() + ". Expected at least 2 arguments.");
		}

		final LispStruct functionForm = input.getRest().getFirst();
		final LispStruct functionFormAnalyzed = analyzer.analyzeForm(functionForm, analysisBuilder);

		final ListStruct forms = input.getRest().getRest();
		final List<LispStruct> formsJavaList = forms.getAsJavaList();
		final List<LispStruct> analyzedForms =
				formsJavaList.stream()
				             .map(e -> analyzer.analyzeForm(e, analysisBuilder))
				             .collect(Collectors.toList());

		return new MultipleValueCallElement(functionFormAnalyzed, analyzedForms);
	}
}
