package jcl.compiler.sa.analyzer.specialoperator;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.Environment;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.struct.specialoperator.CatchStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class CatchExpander extends MacroFunctionExpander<CatchStruct> {

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Override
	public SymbolStruct getFunctionSymbol() {
		return SpecialOperatorStruct.CATCH;
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
