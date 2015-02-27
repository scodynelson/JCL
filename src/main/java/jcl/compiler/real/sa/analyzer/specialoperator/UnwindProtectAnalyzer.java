package jcl.compiler.real.sa.analyzer.specialoperator;

import jcl.compiler.real.element.ConsElement;
import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.SimpleElement;
import jcl.compiler.real.element.specialoperator.UnwindProtectElement;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.system.EnhancedLinkedList;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.stream.Collectors;

@Component
public class UnwindProtectAnalyzer implements SpecialOperatorAnalyzer {

	private static final long serialVersionUID = 3379320303375207710L;

	@Override
	public UnwindProtectElement analyze(final SemanticAnalyzer analyzer, final ConsElement input, final AnalysisBuilder analysisBuilder) {

		final EnhancedLinkedList<SimpleElement> elements = input.getElements();

		final int inputSize = elements.size();
		if (inputSize < 2) {
			throw new ProgramErrorException("UNWIND-PROTECT: Incorrect number of arguments: " + inputSize + ". Expected at least 2 arguments.");
		}

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
