package jcl.compiler.real.sa.analyzer.specialoperator;

import jcl.compiler.real.element.ConsElement;
import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.SimpleElement;
import jcl.compiler.real.element.specialoperator.ThrowElement;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.system.EnhancedLinkedList;
import org.springframework.stereotype.Component;

@Component
public class ThrowAnalyzer implements SpecialOperatorAnalyzer {

	private static final long serialVersionUID = 359191567361134081L;

	@Override
	public ThrowElement analyze(final ConsElement input, final AnalysisBuilder analysisBuilder) {

		final EnhancedLinkedList<SimpleElement> elements = input.getElements();

		final int inputSize = elements.size();
		if (inputSize != 3) {
			throw new ProgramErrorException("THROW: Incorrect number of arguments: " + inputSize + ". Expected 3 arguments.");
		}

		final EnhancedLinkedList<SimpleElement> inputRest = elements.getAllButFirst();

		final SemanticAnalyzer analyzer = analysisBuilder.getAnalyzer();

		final SimpleElement catchTag = inputRest.getFirst();
		final Element catchTagAnalyzed = analyzer.analyzeForm(catchTag, analysisBuilder);

		final EnhancedLinkedList<SimpleElement> inputRestRest = inputRest.getAllButFirst();

		final SimpleElement resultForm = inputRestRest.getFirst();
		final Element resultFormAnalyzed = analyzer.analyzeForm(resultForm, analysisBuilder);

		return new ThrowElement(catchTagAnalyzed, resultFormAnalyzed);
	}
}
