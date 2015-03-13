package jcl.compiler.real.sa.analyzer.specialoperator;

import java.util.List;
import java.util.stream.Collectors;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.sa.FormAnalyzer;
import jcl.compiler.real.sa.analyzer.expander.real.MacroFunctionExpander;
import jcl.compiler.real.struct.specialoperator.UnwindProtectStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class UnwindProtectExpander extends MacroFunctionExpander<UnwindProtectStruct> {

	private static final long serialVersionUID = 3379320303375207710L;

	@Autowired
	private FormAnalyzer formAnalyzer;

	/**
	 * Initializes the block macro function and adds it to the special operator 'block'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperator.UNWIND_PROTECT.setMacroFunctionExpander(this);
	}

	@Override
	public UnwindProtectStruct expand(final ListStruct form, final Environment environment) {

		final int inputSize = form.size();
		if (inputSize < 2) {
			throw new ProgramErrorException("UNWIND-PROTECT: Incorrect number of arguments: " + inputSize + ". Expected at least 2 arguments.");
		}

		final ListStruct inputRest = form.getRest();
		final LispStruct protectedForm = inputRest.getFirst();
		final LispStruct analyzedProtectedForm = formAnalyzer.analyze(protectedForm, environment);

		final List<LispStruct> cleanupForms = inputRest.getRest().getAsJavaList();

		final List<LispStruct> analyzedCleanupForms =
				cleanupForms.stream()
				            .map(e -> formAnalyzer.analyze(e, environment))
				            .collect(Collectors.toList());

		return new UnwindProtectStruct(analyzedProtectedForm, analyzedCleanupForms);
	}
}
