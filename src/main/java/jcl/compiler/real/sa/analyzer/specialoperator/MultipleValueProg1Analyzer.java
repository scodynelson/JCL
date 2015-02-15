package jcl.compiler.real.sa.analyzer.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.specialoperator.MultipleValueProg1Element;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.stream.Collectors;

@Component
public class MultipleValueProg1Analyzer implements SpecialOperatorAnalyzer {

	private static final long serialVersionUID = 1791554561862006171L;

	@Override
	public MultipleValueProg1Element analyze(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder) {

		if (input.size() < 2) {
			throw new ProgramErrorException("MULTIPLE-VALUE-PROG1: Incorrect number of arguments: " + input.size() + ". Expected at least 2 arguments.");
		}

		final LispStruct firstForm = input.getRest().getFirst();
		final Element firstFormAnalyzed = analyzer.analyzeForm(firstForm, analysisBuilder);

		final ListStruct forms = input.getRest().getRest();
		final List<LispStruct> formsJavaList = forms.getAsJavaList();
		final List<Element> analyzedForms =
				formsJavaList.stream()
				             .map(e -> analyzer.analyzeForm(e, analysisBuilder))
				             .collect(Collectors.toList());

		return new MultipleValueProg1Element(firstFormAnalyzed, analyzedForms);
	}
}