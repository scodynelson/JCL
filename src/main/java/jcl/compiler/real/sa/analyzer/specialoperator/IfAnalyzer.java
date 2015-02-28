package jcl.compiler.real.sa.analyzer.specialoperator;

import jcl.compiler.real.element.ConsElement;
import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.SimpleElement;
import jcl.compiler.real.element.specialoperator.IfElement;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.analyzer.expander.real.MacroFunctionExpander;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.symbols.SpecialOperator;
import jcl.system.EnhancedLinkedList;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;

@Component
public class IfAnalyzer extends MacroFunctionExpander implements SpecialOperatorAnalyzer {

	private static final long serialVersionUID = -5414856145190749144L;

	/**
	 * Initializes the block macro function and adds it to the special operator 'block'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperator.IF.setMacroFunctionExpander(this);
	}

	@Override
	public Element expand(final ConsElement form, final AnalysisBuilder analysisBuilder) {
		return analyze(form, analysisBuilder);
	}

	@Override
	public IfElement analyze(final ConsElement input, final AnalysisBuilder analysisBuilder) {

		final EnhancedLinkedList<SimpleElement> elements = input.getElements();

		final int inputSize = elements.size();
		if ((inputSize < 3) || (inputSize > 4)) {
			throw new ProgramErrorException("IF: Incorrect number of arguments: " + inputSize + ". Expected either 3 or 4 arguments.");
		}

		final EnhancedLinkedList<SimpleElement> inputRest = elements.getAllButFirst();

		final SemanticAnalyzer analyzer = analysisBuilder.getAnalyzer();

		final SimpleElement testForm = inputRest.getFirst();
		final Element testFormAnalyzed = analyzer.analyzeForm(testForm, analysisBuilder);

		final EnhancedLinkedList<SimpleElement> inputRestRest = inputRest.getAllButFirst();

		final SimpleElement thenForm = inputRestRest.getFirst();
		final Element thenFormAnalyzed = analyzer.analyzeForm(thenForm, analysisBuilder);

		if (inputSize == 4) {
			final EnhancedLinkedList<SimpleElement> inputRestRestRest = inputRestRest.getAllButFirst();

			final SimpleElement elseForm = inputRestRestRest.getFirst();
			final Element elseFormAnalyzed = analyzer.analyzeForm(elseForm, analysisBuilder);
			return new IfElement(testFormAnalyzed, thenFormAnalyzed, elseFormAnalyzed);
		} else {
			return new IfElement(testFormAnalyzed, thenFormAnalyzed);
		}
	}
}
