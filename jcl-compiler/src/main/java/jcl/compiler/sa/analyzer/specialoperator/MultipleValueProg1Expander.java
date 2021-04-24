package jcl.compiler.sa.analyzer.specialoperator;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import jcl.compiler.environment.Environment;
import jcl.compiler.function.expanders.MacroFunctionExpander;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.struct.specialoperator.MultipleValueProg1Struct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.ProgramErrorException;
import jcl.lang.statics.CommonLispSymbols;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class MultipleValueProg1Expander extends MacroFunctionExpander<MultipleValueProg1Struct> {

	public static final MultipleValueProg1Expander INSTANCE = new MultipleValueProg1Expander();

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.MULTIPLE_VALUE_PROG1;
	}

	@Override
	public MultipleValueProg1Struct expand(final ListStruct form, final Environment environment) {
		final Iterator<LispStruct> iterator = form.iterator();
		iterator.next(); // MULTIPLE-VALUE-PROG1 SYMBOL

		if (!iterator.hasNext()) {
			throw new ProgramErrorException("MULTIPLE-VALUE-PROG1: Incorrect number of arguments: 0. Expected at least 1 argument.");
		}
		final LispStruct first = iterator.next();
		final LispStruct firstForm = FormAnalyzer.analyze(first, environment);

		final List<LispStruct> forms = new ArrayList<>();
		iterator.forEachRemaining(element -> {
			final LispStruct analyzedElement = FormAnalyzer.analyze(element, environment);
			forms.add(analyzedElement);
		});
		return new MultipleValueProg1Struct(firstForm, forms);
	}
}
