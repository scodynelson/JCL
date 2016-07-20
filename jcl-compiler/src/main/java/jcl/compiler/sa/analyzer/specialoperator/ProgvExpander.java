package jcl.compiler.sa.analyzer.specialoperator;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import jcl.compiler.environment.Environment;
import jcl.compiler.environment.ProgvEnvironment;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.struct.specialoperator.ProgvStruct;
import jcl.compiler.function.expanders.MacroFunctionExpander;
import jcl.lang.CommonLispSymbols;
import jcl.lang.LispStruct;
import jcl.lang.list.ListStruct;
import jcl.lang.SpecialOperatorStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.ProgramErrorException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class ProgvExpander extends MacroFunctionExpander<ProgvStruct> {

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Override
	public SymbolStruct getFunctionSymbol() {
		return SpecialOperatorStruct.PROGV;
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

		final ListStruct quotedVars = ListStruct.buildProperList(SpecialOperatorStruct.QUOTE, first);
		final ListStruct evalVars = ListStruct.buildProperList(CommonLispSymbols.EVAL, quotedVars);
		final LispStruct analyzedEvalVars = formAnalyzer.analyze(evalVars, environment);

		final ListStruct quotedVals = ListStruct.buildProperList(SpecialOperatorStruct.QUOTE, second);
		final ListStruct evalVals = ListStruct.buildProperList(CommonLispSymbols.EVAL, quotedVals);
		final LispStruct analyzedEvalVals = formAnalyzer.analyze(evalVals, environment);

		// Handle Progn Environment processing
		final ProgvEnvironment progvEnvironment = new ProgvEnvironment(environment);

		final List<LispStruct> forms = new ArrayList<>();
		iterator.forEachRemaining(element -> {
			final LispStruct analyzedElement = formAnalyzer.analyze(element, environment);
			forms.add(analyzedElement);
		});
		return new ProgvStruct(analyzedEvalVars, analyzedEvalVals, forms, progvEnvironment);
	}
}
