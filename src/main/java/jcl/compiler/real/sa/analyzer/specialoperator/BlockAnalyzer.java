package jcl.compiler.real.sa.analyzer.specialoperator;

import jcl.compiler.real.element.ConsElement;
import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.SimpleElement;
import jcl.compiler.real.element.SymbolElement;
import jcl.compiler.real.element.specialoperator.BlockElement;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.system.EnhancedLinkedList;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.stream.Collectors;

@Component
public class BlockAnalyzer implements SpecialOperatorAnalyzer {

	private static final long serialVersionUID = -5185467468586381117L;

	@Override
	public BlockElement analyze(final SemanticAnalyzer analyzer, final ConsElement input, final AnalysisBuilder analysisBuilder) {

		final EnhancedLinkedList<SimpleElement> elements = input.getElements();

		final int inputSize = elements.size();
		if (inputSize < 2) {
			throw new ProgramErrorException("BLOCK: Incorrect number of arguments: " + inputSize + ". Expected at least 2 arguments.");
		}

		final EnhancedLinkedList<SimpleElement> inputRest = elements.getAllButFirst();

		final SimpleElement second = inputRest.getFirst();
		if (!(second instanceof SymbolElement)) {
			throw new ProgramErrorException("BLOCK: Label must be of type SymbolStruct. Got: " + second);
		}

		final SymbolElement name = (SymbolElement) second;
		analysisBuilder.getBlockStack().push(name);

		try {
			final EnhancedLinkedList<SimpleElement> forms = inputRest.getAllButFirst();

			final List<Element> analyzedForms =
					forms.stream()
					     .map(e -> analyzer.analyzeForm(e, analysisBuilder))
					     .collect(Collectors.toList());

			return new BlockElement(name, analyzedForms);
		} finally {
			analysisBuilder.getBlockStack().pop();
		}
	}
}
