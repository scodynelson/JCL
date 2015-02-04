package jcl.compiler.real.sa.analyzer.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.element.specialoperator.PrognElement;
import jcl.lists.ListStruct;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.stream.Collectors;

@Component
public class PrognAnalyzer implements SpecialOperatorAnalyzer {

	private static final long serialVersionUID = -2851059577992887882L;

	@Override
	public PrognElement analyze(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder) {

		final ListStruct forms = input.getRest();

		final List<LispStruct> formsJavaList = forms.getAsJavaList();
		final List<LispStruct> analyzedForms =
				formsJavaList.stream()
				             .map(e -> analyzer.analyzeForm(e, analysisBuilder))
				             .collect(Collectors.toList());

		return new PrognElement(analyzedForms);
	}
}
