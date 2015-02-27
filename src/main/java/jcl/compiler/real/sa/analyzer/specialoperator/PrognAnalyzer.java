package jcl.compiler.real.sa.analyzer.specialoperator;

import jcl.compiler.real.element.ConsElement;
import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.SimpleElement;
import jcl.compiler.real.element.specialoperator.PrognElement;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.system.EnhancedLinkedList;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.stream.Collectors;

@Component
public class PrognAnalyzer implements SpecialOperatorAnalyzer {

	private static final long serialVersionUID = -2851059577992887882L;

	@Override
	public PrognElement analyze(final SemanticAnalyzer analyzer, final ConsElement input, final AnalysisBuilder analysisBuilder) {

		final EnhancedLinkedList<SimpleElement> elements = input.getElements();

		final EnhancedLinkedList<SimpleElement> forms = elements.getAllButFirst();

		final List<Element> analyzedForms =
				forms.stream()
				     .map(e -> analyzer.analyzeForm(e, analysisBuilder))
				     .collect(Collectors.toList());

		return new PrognElement(analyzedForms);
	}
}
