package jcl.compiler.sa.analyzer.specialoperator;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import jcl.compiler.environment.Environment;
import jcl.compiler.function.expanders.MacroFunctionExpander;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.struct.specialoperator.CatchStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.ProgramErrorException;
import jcl.lang.internal.SpecialOperatorStructImpl;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class CatchExpander extends MacroFunctionExpander<CatchStruct> {

	public static final CatchExpander INSTANCE = new CatchExpander();

	@Override
	public SymbolStruct getFunctionSymbol() {
		return SpecialOperatorStructImpl.CATCH;
	}

	@Override
	public CatchStruct expand(final ListStruct form, final Environment environment) {
		final Iterator<LispStruct> iterator = form.iterator();
		iterator.next(); // CATCH SYMBOL

		if (!iterator.hasNext()) {
			throw new ProgramErrorException("CATCH: Incorrect number of arguments: 0. Expected at least 1 argument.");
		}
		final LispStruct catchTag = iterator.next();
		final LispStruct catchTagAnalyzed = FormAnalyzer.analyze(catchTag, environment);

		final List<LispStruct> forms = new ArrayList<>();
		iterator.forEachRemaining(element -> {
			final LispStruct analyzedElement = FormAnalyzer.analyze(element, environment);
			forms.add(analyzedElement);
		});
		return new CatchStruct(catchTagAnalyzed, forms);
	}
}
