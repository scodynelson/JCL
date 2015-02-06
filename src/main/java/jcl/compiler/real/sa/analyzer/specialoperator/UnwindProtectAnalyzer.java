package jcl.compiler.real.sa.analyzer.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.specialoperator.UnwindProtectElement;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.stream.Collectors;

@Component
public class UnwindProtectAnalyzer implements SpecialOperatorAnalyzer {

	private static final long serialVersionUID = 3379320303375207710L;

	@Override
	public UnwindProtectElement analyze(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder) {

		final int inputSize = input.size();
		if (inputSize < 2) {
			throw new ProgramErrorException("UNWIND-PROTECT: Incorrect number of arguments: " + inputSize + ". Expected at least 2 arguments.");
		}

		final LispStruct protectedForm = input.getRest().getFirst();
		final Element analyzedProtectedForm = analyzer.analyzeForm(protectedForm, analysisBuilder);

		final ListStruct cleanupForms = input.getRest().getRest();

		final List<LispStruct> cleanupFormsJavaList = cleanupForms.getAsJavaList();
		final List<Element> analyzedCleanupForms =
				cleanupFormsJavaList.stream()
				                    .map(e -> analyzer.analyzeForm(e, analysisBuilder))
				                    .collect(Collectors.toList());

		return new UnwindProtectElement(analyzedProtectedForm, analyzedCleanupForms);
	}
}
