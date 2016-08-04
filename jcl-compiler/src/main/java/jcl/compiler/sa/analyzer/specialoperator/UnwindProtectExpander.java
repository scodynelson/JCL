package jcl.compiler.sa.analyzer.specialoperator;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import jcl.compiler.environment.Environment;
import jcl.compiler.function.expanders.MacroFunctionExpander;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.struct.specialoperator.UnwindProtectStruct;
import jcl.lang.LispStruct;
import jcl.lang.internal.SpecialOperatorStructImpl;
import jcl.lang.SymbolStructImpl;
import jcl.lang.condition.exception.ProgramErrorException;
import jcl.lang.ListStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class UnwindProtectExpander extends MacroFunctionExpander<UnwindProtectStruct> {

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Override
	public SymbolStructImpl getFunctionSymbol() {
		return SpecialOperatorStructImpl.UNWIND_PROTECT;
	}

	@Override
	public UnwindProtectStruct expand(final ListStruct form, final Environment environment) {
		final Iterator<LispStruct> iterator = form.iterator();
		iterator.next(); // UNWIND-PROTECT SYMBOL

		if (!iterator.hasNext()) {
			throw new ProgramErrorException("UNWIND-PROTECT: Incorrect number of arguments: 0. Expected at least 1 argument.");
		}
		final LispStruct first = iterator.next();
		final LispStruct protectedForm = formAnalyzer.analyze(first, environment);

		final List<LispStruct> cleanupForms = new ArrayList<>();
		iterator.forEachRemaining(element -> {
			final LispStruct analyzedElement = formAnalyzer.analyze(element, environment);
			cleanupForms.add(analyzedElement);
		});
		return new UnwindProtectStruct(protectedForm, cleanupForms);
	}
}
