package jcl.compiler.real.sa.analyzer.specialoperator;

import java.util.List;
import java.util.stream.Collectors;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.sa.FormAnalyzer;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import jcl.compiler.real.struct.specialoperator.UnwindProtectStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperatorStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class UnwindProtectExpander extends MacroFunctionExpander<UnwindProtectStruct> {

	private static final long serialVersionUID = 3379320303375207710L;

	@Autowired
	private FormAnalyzer formAnalyzer;

	/**
	 * Initializes the unwind-protect macro function and adds it to the special operator 'unwind-protect'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperatorStruct.UNWIND_PROTECT.setMacroFunctionExpander(this);
	}

	@Override
	public UnwindProtectStruct expand(final ListStruct form, final Environment environment) {

		final int formSize = form.size();
		if (formSize < 2) {
			throw new ProgramErrorException("UNWIND-PROTECT: Incorrect number of arguments: " + formSize + ". Expected at least 2 arguments.");
		}

		final ListStruct formRest = form.getRest();

		final LispStruct protectedForm = formRest.getFirst();
		final LispStruct analyzedProtectedForm = formAnalyzer.analyze(protectedForm, environment);

		final ListStruct formRestRest = formRest.getRest();

		final List<LispStruct> cleanupForms = formRestRest.getAsJavaList();
		final List<LispStruct> analyzedCleanupForms =
				cleanupForms.stream()
				            .map(e -> formAnalyzer.analyze(e, environment))
				            .collect(Collectors.toList());

		return new UnwindProtectStruct(analyzedProtectedForm, new PrognStruct(analyzedCleanupForms));
	}
}
