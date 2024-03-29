package jcl.compiler.sa.analyzer.specialoperator;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import jcl.compiler.environment.Environment;
import jcl.compiler.function.expanders.MacroFunctionExpander;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.struct.specialoperator.UnwindProtectStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.ProgramErrorException;
import jcl.lang.statics.CommonLispSymbols;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class UnwindProtectExpander extends MacroFunctionExpander<UnwindProtectStruct> {

	public static final UnwindProtectExpander INSTANCE = new UnwindProtectExpander();

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.UNWIND_PROTECT;
	}

	@Override
	public UnwindProtectStruct expand(final ListStruct form, final Environment environment) {
		final Iterator<LispStruct> iterator = form.iterator();
		iterator.next(); // UNWIND-PROTECT SYMBOL

		if (!iterator.hasNext()) {
			throw new ProgramErrorException("UNWIND-PROTECT: Incorrect number of arguments: 0. Expected at least 1 argument.");
		}
		final LispStruct first = iterator.next();
		final LispStruct protectedForm = FormAnalyzer.analyze(first, environment);

		final List<LispStruct> cleanupForms = new ArrayList<>();
		iterator.forEachRemaining(element -> {
			final LispStruct analyzedElement = FormAnalyzer.analyze(element, environment);
			cleanupForms.add(analyzedElement);
		});
		return new UnwindProtectStruct(protectedForm, cleanupForms);
	}
}
