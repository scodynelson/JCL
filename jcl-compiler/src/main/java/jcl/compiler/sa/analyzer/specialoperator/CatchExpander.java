package jcl.compiler.sa.analyzer.specialoperator;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import jcl.compiler.environment.Environment;
import jcl.compiler.function.expanders.MacroFunctionExpander;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.struct.specialoperator.CatchStruct;
import jcl.lang.LispStruct;
import jcl.lang.internal.SpecialOperatorStructImpl;
import jcl.lang.SymbolStructImpl;
import jcl.lang.condition.exception.ProgramErrorException;
import jcl.lang.ListStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class CatchExpander extends MacroFunctionExpander<CatchStruct> {

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Override
	public SymbolStructImpl getFunctionSymbol() {
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
		final LispStruct catchTagAnalyzed = formAnalyzer.analyze(catchTag, environment);

		final List<LispStruct> forms = new ArrayList<>();
		iterator.forEachRemaining(element -> {
			final LispStruct analyzedElement = formAnalyzer.analyze(element, environment);
			forms.add(analyzedElement);
		});
		return new CatchStruct(catchTagAnalyzed, forms);
	}
}
