package jcl.compiler.real.sa.analyzer.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.element.UnwindProtectElement;
import jcl.compiler.real.sa.analyzer.specialoperator.body.BodyAnalyzer;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
public class UnwindProtectAnalyzer implements SpecialOperatorAnalyzer {

	private static final long serialVersionUID = 3379320303375207710L;

	@Autowired
	private BodyAnalyzer bodyAnalyzer;

	@Override
	public UnwindProtectElement analyze(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder) {

		final int inputSize = input.size();
		if (inputSize < 2) {
			throw new ProgramErrorException("UNWIND-PROTECT: Incorrect number of arguments: " + inputSize + ". Expected at least 2 arguments.");
		}

		final LispStruct protectedForm = input.getRest().getFirst();
		final LispStruct analyzedProtectedForm = analyzer.analyzeForm(protectedForm, analysisBuilder);

		final ListStruct cleanupForms = input.getRest().getRest();
		final List<LispStruct> analyzedCleanupForms = bodyAnalyzer.analyze(analyzer, cleanupForms, analysisBuilder);

		return new UnwindProtectElement(analyzedProtectedForm, analyzedCleanupForms);
	}
}
