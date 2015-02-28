package jcl.compiler.real.sa.analyzer.specialoperator;

import jcl.compiler.real.element.ConsElement;
import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.SimpleElement;
import jcl.compiler.real.element.specialoperator.UnwindProtectElement;
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
public class UnwindProtectAnalyzer extends MacroFunctionExpander implements SpecialOperatorAnalyzer {

	private static final long serialVersionUID = 3379320303375207710L;

	/**
	 * Initializes the block macro function and adds it to the special operator 'block'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperator.UNWIND_PROTECT.setMacroFunctionExpander(this);
	}

	@Override
	public Element expand(final ConsElement form, final AnalysisBuilder analysisBuilder) {
		return analyze(form, analysisBuilder);
	}

	@Override
	public UnwindProtectElement analyze(final ConsElement input, final AnalysisBuilder analysisBuilder) {

		final EnhancedLinkedList<SimpleElement> elements = input.getElements();

		final int inputSize = elements.size();
		if (inputSize < 2) {
			throw new ProgramErrorException("UNWIND-PROTECT: Incorrect number of arguments: " + inputSize + ". Expected at least 2 arguments.");
		}

		final SemanticAnalyzer analyzer = analysisBuilder.getAnalyzer();

		final EnhancedLinkedList<SimpleElement> inputRest = elements.getAllButFirst();
		final SimpleElement protectedForm = inputRest.getFirst();
		final Element analyzedProtectedForm = analyzer.analyzeForm(protectedForm, analysisBuilder);

		final EnhancedLinkedList<SimpleElement> cleanupForms = inputRest.getAllButFirst();

		final List<Element> analyzedCleanupForms =
				cleanupForms.stream()
				            .map(e -> analyzer.analyzeForm(e, analysisBuilder))
				            .collect(Collectors.toList());

		return new UnwindProtectElement(analyzedProtectedForm, analyzedCleanupForms);
	}
}
