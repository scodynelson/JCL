package jcl.compiler.real.sa.analyzer.specialoperator;

import jcl.compiler.real.element.ConsElement;
import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.ListElement;
import jcl.compiler.real.element.SimpleElement;
import jcl.compiler.real.element.SymbolElement;
import jcl.compiler.real.element.specialoperator.TheElement;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.system.EnhancedLinkedList;
import org.springframework.stereotype.Component;

@Component
public class TheAnalyzer implements SpecialOperatorAnalyzer {

	private static final long serialVersionUID = 6723289642694216454L;

	@Override
	public TheElement analyze(final ConsElement input, final AnalysisBuilder analysisBuilder) {

		final EnhancedLinkedList<SimpleElement> elements = input.getElements();

		final int inputSize = elements.size();
		if (inputSize != 3) {
			throw new ProgramErrorException("THE: Incorrect number of arguments: " + inputSize + ". Expected 3 arguments.");
		}

		final EnhancedLinkedList<SimpleElement> inputRest = elements.getAllButFirst();

		final SimpleElement valueType = inputRest.getFirst();
		if (!(valueType instanceof SymbolElement) && !(valueType instanceof ListElement)) {
			throw new ProgramErrorException("THE: Type specifier must be of type SymbolStruct or ListStruct. Got: " + valueType);
		}
		// TODO: do we actually want to somehow factor in the 'TypeSpecifier' produced by the second value?

		final EnhancedLinkedList<SimpleElement> inputRestRest = inputRest.getAllButFirst();

		final SemanticAnalyzer analyzer = analysisBuilder.getAnalyzer();

		final SimpleElement form = inputRestRest.getFirst();
		final Element formAnalyzed = analyzer.analyzeForm(form, analysisBuilder);

		return new TheElement(null, formAnalyzed);
	}
}
