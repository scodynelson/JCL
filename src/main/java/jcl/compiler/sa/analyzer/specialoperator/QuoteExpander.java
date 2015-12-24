package jcl.compiler.sa.analyzer.specialoperator;

import jcl.LispStruct;
import jcl.compiler.environment.Environment;
import jcl.compiler.sa.analyzer.LispFormValueValidator;
import jcl.compiler.struct.specialoperator.QuoteStruct;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class QuoteExpander extends MacroFunctionExpander<QuoteStruct> {

	private static final long serialVersionUID = 2741011595927247743L;

	@Autowired
	private LispFormValueValidator validator;

	@Override
	public SymbolStruct getFunctionSymbol() {
		return SpecialOperatorStruct.QUOTE;
	}

	@Override
	public QuoteStruct expand(final ListStruct form, final Environment environment) {
		validator.validateListFormSizeExact(form, 2, "QUOTE");

		final ListStruct formRest = form.getRest();

		final LispStruct quotedObject = formRest.getFirst();
		return new QuoteStruct(quotedObject);
	}
}
