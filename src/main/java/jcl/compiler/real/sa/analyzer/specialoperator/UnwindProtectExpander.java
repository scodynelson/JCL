package jcl.compiler.real.sa.analyzer.specialoperator;

import java.util.List;
import java.util.stream.Collectors;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.sa.FormAnalyzer;
import jcl.compiler.real.sa.analyzer.LispFormValueValidator;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import jcl.compiler.real.struct.specialoperator.UnwindProtectStruct;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class UnwindProtectExpander extends MacroFunctionExpander<UnwindProtectStruct> {

	private static final long serialVersionUID = 3379320303375207710L;

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Autowired
	private LispFormValueValidator validator;

	@Override
	public SymbolStruct<?> getFunctionSymbol() {
		return SpecialOperatorStruct.UNWIND_PROTECT;
	}

	@Override
	public UnwindProtectStruct expand(final ListStruct form, final Environment environment) {
		validator.validateListFormSize(form, 2, "UNWIND-PROTECT");

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
