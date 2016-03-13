package jcl.compiler.sa.analyzer.specialoperator;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.Environment;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.struct.specialoperator.MultipleValueProg1Struct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class MultipleValueProg1Expander extends MacroFunctionExpander<MultipleValueProg1Struct> {

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Override
	public SymbolStruct getFunctionSymbol() {
		return SpecialOperatorStruct.MULTIPLE_VALUE_PROG1;
	}

	@Override
	public MultipleValueProg1Struct expand(final ListStruct form, final Environment environment) {
		final Iterator<LispStruct> iterator = form.iterator();
		iterator.next(); // MULTIPLE-VALUE-PROG1 SYMBOL

		if (!iterator.hasNext()) {
			throw new ProgramErrorException("MULTIPLE-VALUE-PROG1: Incorrect number of arguments: 0. Expected at least 1 argument.");
		}
		final LispStruct first = iterator.next();
		final LispStruct firstForm = formAnalyzer.analyze(first, environment);

		final List<LispStruct> forms = new ArrayList<>();
		iterator.forEachRemaining(element -> {
			final LispStruct analyzedElement = formAnalyzer.analyze(element, environment);
			forms.add(analyzedElement);
		});
		return new MultipleValueProg1Struct(firstForm, forms);
	}
}
