package jcl.compiler.real.sa.analyzer.specialoperator;

import jcl.compiler.real.element.ConsElement;
import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.SimpleElement;
import jcl.compiler.real.element.SymbolElement;
import jcl.compiler.real.element.specialoperator.ReturnFromElement;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.system.EnhancedLinkedList;
import org.springframework.stereotype.Component;

@Component
public class ReturnFromAnalyzer implements SpecialOperatorAnalyzer {

	private static final long serialVersionUID = 3328790948675693554L;

	@Override
	public ReturnFromElement analyze(final ConsElement input, final AnalysisBuilder analysisBuilder) {

		final EnhancedLinkedList<SimpleElement> elements = input.getElements();

		final int inputSize = elements.size();
		if ((inputSize < 2) || (inputSize > 3)) {
			throw new ProgramErrorException("RETURN-FROM: Incorrect number of arguments: " + inputSize + ". Expected either 2 or 3 arguments.");
		}

		final EnhancedLinkedList<SimpleElement> inputRest = elements.getAllButFirst();

		final SimpleElement second = inputRest.getFirst();
		if (!(second instanceof SymbolElement)) {
			throw new ProgramErrorException("RETURN-FROM: Name must be of type SymbolStruct. Got: " + second);
		}

		final SymbolElement name = (SymbolElement) second;

		if (analysisBuilder.getBlockStack().search(name) == -1) {
			throw new ProgramErrorException("RETURN-FROM: No BLOCK with name " + second + " is visible.");
		}

		if (inputSize == 3) {
			final EnhancedLinkedList<SimpleElement> inputRestRest = inputRest.getAllButFirst();

			final SemanticAnalyzer analyzer = analysisBuilder.getAnalyzer();

			final SimpleElement result = inputRestRest.getFirst();
			final Element analyzedResult = analyzer.analyzeForm(result, analysisBuilder);
			return new ReturnFromElement(name, analyzedResult);
		} else {
			return new ReturnFromElement(name);
		}
	}
}
