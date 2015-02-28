package jcl.compiler.real.sa.analyzer.specialoperator;

import jcl.compiler.real.element.ConsElement;
import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.SimpleElement;
import jcl.compiler.real.element.SymbolElement;
import jcl.compiler.real.element.specialoperator.BlockElement;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.symbols.SpecialOperator;
import jcl.system.EnhancedLinkedList;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import java.util.List;
import java.util.stream.Collectors;

@Component
public class BlockAnalyzer implements SpecialOperatorAnalyzer {

	private static final long serialVersionUID = -5185467468586381117L;

	/**
	 * Initializes the block macro function and adds it to the special operator 'block'.
	 */
	@PostConstruct
	private void init() {
//		SpecialOperator.BLOCK.setMacroFunctionExpander(this);
	}

	@Override
	public BlockElement analyze(final ConsElement input, final AnalysisBuilder analysisBuilder) {

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

			final SemanticAnalyzer analyzer = analysisBuilder.getAnalyzer();

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
