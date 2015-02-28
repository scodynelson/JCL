package jcl.compiler.real.sa.analyzer.specialoperator;

import jcl.compiler.real.element.ConsElement;
import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.SimpleElement;
import jcl.compiler.real.element.specialoperator.MultipleValueCallElement;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.analyzer.expander.real.MacroFunctionExpander;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.symbols.SpecialOperator;
import jcl.system.EnhancedLinkedList;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import java.util.List;
import java.util.stream.Collectors;

@Component
public class MultipleValueCallAnalyzer extends MacroFunctionExpander implements SpecialOperatorAnalyzer {

	private static final long serialVersionUID = -8350781874747372684L;

	/**
	 * Initializes the block macro function and adds it to the special operator 'block'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperator.MULTIPLE_VALUE_CALL.setMacroFunctionExpander(this);
	}

	@Override
	public Element expand(final ConsElement form, final AnalysisBuilder analysisBuilder) {
		return analyze(form, analysisBuilder);
	}

	@Override
	public MultipleValueCallElement analyze(final ConsElement input, final AnalysisBuilder analysisBuilder) {

		final EnhancedLinkedList<SimpleElement> elements = input.getElements();

		final int inputSize = elements.size();
		if (inputSize < 2) {
			throw new ProgramErrorException("MULTIPLE-VALUE-CALL: Incorrect number of arguments: " + inputSize + ". Expected at least 2 arguments.");
		}

		final EnhancedLinkedList<SimpleElement> inputRest = elements.getAllButFirst();

		final SemanticAnalyzer analyzer = analysisBuilder.getAnalyzer();

		final SimpleElement functionForm = inputRest.getFirst();
		final Element functionFormAnalyzed = analyzer.analyzeForm(functionForm, analysisBuilder);

		final EnhancedLinkedList<SimpleElement> forms = inputRest.getAllButFirst();
		final List<Element> analyzedForms =
				forms.stream()
				     .map(e -> analyzer.analyzeForm(e, analysisBuilder))
				     .collect(Collectors.toList());

		return new MultipleValueCallElement(functionFormAnalyzed, analyzedForms);
	}
}
