package jcl.compiler.sa.analyzer.specialoperator;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import jcl.compiler.environment.Environment;
import jcl.compiler.environment.ProgvEnvironment;
import jcl.compiler.function.expanders.MacroFunctionExpander;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.struct.specialoperator.ProgvStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.ProgramErrorException;
import jcl.lang.statics.CommonLispSymbols;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class ProgvExpander extends MacroFunctionExpander<ProgvStruct> {

	public static final ProgvExpander INSTANCE = new ProgvExpander();

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.PROGV;
	}

	@Override
	public ProgvStruct expand(final ListStruct form, final Environment environment) {
		final Iterator<LispStruct> iterator = form.iterator();
		iterator.next(); // PROGV SYMBOL

		if (!iterator.hasNext()) {
			throw new ProgramErrorException("PROGV: Incorrect number of arguments: 0. Expected at least 2 arguments.");
		}
		final LispStruct first = iterator.next();

		if (!iterator.hasNext()) {
			throw new ProgramErrorException("PROGV: Incorrect number of arguments: 1. Expected at least 2 arguments.");
		}
		final LispStruct second = iterator.next();

		final ListStruct quotedVars = ListStruct.toLispList(CommonLispSymbols.QUOTE, first);
		final ListStruct evalVars = ListStruct.toLispList(CommonLispSymbols.EVAL, quotedVars);
		final LispStruct analyzedEvalVars = FormAnalyzer.analyze(evalVars, environment);

		final ListStruct quotedVals = ListStruct.toLispList(CommonLispSymbols.QUOTE, second);
		final ListStruct evalVals = ListStruct.toLispList(CommonLispSymbols.EVAL, quotedVals);
		final LispStruct analyzedEvalVals = FormAnalyzer.analyze(evalVals, environment);

		// Handle Progn Environment processing
		final ProgvEnvironment progvEnvironment = new ProgvEnvironment(environment);

		final List<LispStruct> forms = new ArrayList<>();
		iterator.forEachRemaining(element -> {
			final LispStruct analyzedElement = FormAnalyzer.analyze(element, environment);
			forms.add(analyzedElement);
		});
		return new ProgvStruct(analyzedEvalVars, analyzedEvalVals, forms, progvEnvironment);
	}
}
