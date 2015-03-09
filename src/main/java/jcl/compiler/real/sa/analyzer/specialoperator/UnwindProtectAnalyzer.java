package jcl.compiler.real.sa.analyzer.specialoperator;

import java.util.List;
import java.util.stream.Collectors;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.analyzer.expander.real.MacroFunctionExpander;
import jcl.compiler.real.struct.specialoperator.UnwindProtectStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;
import org.springframework.stereotype.Component;

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
	public LispStruct expand(final ListStruct form, final AnalysisBuilder analysisBuilder) {
		return analyze(form, analysisBuilder);
	}

	@Override
	public UnwindProtectStruct analyze(final ListStruct input, final AnalysisBuilder analysisBuilder) {

		final int inputSize = input.size();
		if (inputSize < 2) {
			throw new ProgramErrorException("UNWIND-PROTECT: Incorrect number of arguments: " + inputSize + ". Expected at least 2 arguments.");
		}

		final SemanticAnalyzer analyzer = analysisBuilder.getAnalyzer();

		final ListStruct inputRest = input.getRest();
		final LispStruct protectedForm = inputRest.getFirst();
		final LispStruct analyzedProtectedForm = analyzer.analyzeForm(protectedForm, analysisBuilder);

		final List<LispStruct> cleanupForms = inputRest.getRest().getAsJavaList();

		final List<LispStruct> analyzedCleanupForms =
				cleanupForms.stream()
				            .map(e -> analyzer.analyzeForm(e, analysisBuilder))
				            .collect(Collectors.toList());

		return new UnwindProtectStruct(analyzedProtectedForm, analyzedCleanupForms);
	}
}
