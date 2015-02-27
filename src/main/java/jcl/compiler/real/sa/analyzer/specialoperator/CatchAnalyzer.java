package jcl.compiler.real.sa.analyzer.specialoperator;

import jcl.compiler.real.element.ConsElement;
import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.SimpleElement;
import jcl.compiler.real.element.specialoperator.CatchElement;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.system.EnhancedLinkedList;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.stream.Collectors;

@Component
public class CatchAnalyzer implements SpecialOperatorAnalyzer {

	private static final long serialVersionUID = -4421664278117234704L;

	@Override
	public CatchElement analyze(final SemanticAnalyzer analyzer, final ConsElement input, final AnalysisBuilder analysisBuilder) {

		final EnhancedLinkedList<SimpleElement> elements = input.getElements();

		final int inputSize = elements.size();
		if (inputSize < 2) {
			throw new ProgramErrorException("CATCH: Incorrect number of arguments: " + inputSize + ". Expected at least 2 arguments.");
		}

		final EnhancedLinkedList<SimpleElement> inputRest = elements.getAllButFirst();

		final SimpleElement catchTag = inputRest.getFirst();
		final Element catchTagAnalyzed = analyzer.analyzeForm(catchTag, analysisBuilder);

		final EnhancedLinkedList<SimpleElement> forms = inputRest.getAllButFirst();

		final List<Element> analyzedForms =
				forms.stream()
				     .map(e -> analyzer.analyzeForm(e, analysisBuilder))
				     .collect(Collectors.toList());

		return new CatchElement(catchTagAnalyzed, analyzedForms);
	}
}
