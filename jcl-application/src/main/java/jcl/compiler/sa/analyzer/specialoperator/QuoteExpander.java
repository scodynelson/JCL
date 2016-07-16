package jcl.compiler.sa.analyzer.specialoperator;

import java.util.Iterator;

import jcl.LispStruct;
import jcl.compiler.environment.Environment;
import jcl.compiler.struct.specialoperator.QuoteStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public class QuoteExpander extends MacroFunctionExpander<QuoteStruct> {

	@Override
	public SymbolStruct getFunctionSymbol() {
		return SpecialOperatorStruct.QUOTE;
	}

	@Override
	public QuoteStruct expand(final ListStruct form, final Environment environment) {
		final Iterator<LispStruct> iterator = form.iterator();
		iterator.next(); // QUOTE SYMBOL

		if (!iterator.hasNext()) {
			throw new ProgramErrorException("QUOTE: Incorrect number of arguments: 0. Expected 1 argument.");
		}
		final LispStruct quotedObject = iterator.next();

		if (iterator.hasNext()) {
			throw new ProgramErrorException("QUOTE: Incorrect number of arguments: 2. Expected 1 argument.");
		}
		return new QuoteStruct(quotedObject);
	}
}
