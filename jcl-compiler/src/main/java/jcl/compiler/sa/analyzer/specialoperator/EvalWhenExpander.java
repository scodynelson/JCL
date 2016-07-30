package jcl.compiler.sa.analyzer.specialoperator;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import jcl.compiler.environment.Environment;
import jcl.compiler.function.expanders.MacroFunctionExpander;
import jcl.compiler.functions.EvalFunction;
import jcl.lang.CommonLispSymbols;
import jcl.lang.CompilerVariables;
import jcl.lang.LispStruct;
import jcl.lang.SpecialOperatorStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.ProgramErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.list.ConsStruct;
import jcl.lang.list.ListStruct;
import jcl.lang.list.NILStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class EvalWhenExpander extends MacroFunctionExpander<LispStruct> {

	private static final Set<SymbolStruct> SITUATION_KEYWORDS = new HashSet<>(6);

	static {
		SITUATION_KEYWORDS.add(CommonLispSymbols.COMPILE_TOPLEVEL);
		SITUATION_KEYWORDS.add(CommonLispSymbols.LOAD_TOPLEVEL);
		SITUATION_KEYWORDS.add(CommonLispSymbols.EXECUTE);

		SITUATION_KEYWORDS.add(CommonLispSymbols.COMPILE);
		SITUATION_KEYWORDS.add(CommonLispSymbols.LOAD);
		SITUATION_KEYWORDS.add(CommonLispSymbols.EVAL);
	}

	@Autowired
	private EvalFunction evalFunction;

	@Override
	public SymbolStruct getFunctionSymbol() {
		return SpecialOperatorStruct.EVAL_WHEN;
	}

	@Override
	public LispStruct expand(final ListStruct form, final Environment environment) {
		final Iterator<LispStruct> iterator = form.iterator();
		iterator.next(); // EVAL-WHEN SYMBOL

		if (!iterator.hasNext()) {
			throw new ProgramErrorException("EVAL-WHEN: Incorrect number of arguments: 0. Expected at least 1 argument.");
		}
		final LispStruct first = iterator.next();

		if (!(first instanceof ListStruct)) {
			throw new TypeErrorException("EVAL-WHEN: SITUATION-LIST must be a List. Got: " + first);
		}
		final ListStruct situationList = (ListStruct) first;

		final boolean nonSituationsFound = !situationList.stream().allMatch(SITUATION_KEYWORDS::contains);
		if (nonSituationsFound) {
			throw new ProgramErrorException("EVAL-WHEN: Situations must be one of ':COMPILE-TOPLEVEL', ':LOAD-TOPLEVEL', ':EXECUTE', 'COMPILE', 'LOAD', or 'EVAL'. Got: " + situationList);
		}

		final List<LispStruct> forms = new ArrayList<>();
		iterator.forEachRemaining(forms::add);

		final boolean isTopLevel = CompilerVariables.COMPILE_TOP_LEVEL.getVariableValue().booleanValue();
		final boolean convertingForCompiler = !CompilerVariables.CONVERTING_FOR_INTERPRETER.getVariableValue().booleanValue();

		if (isTopLevel) {
			if (isCompileTopLevel(situationList)) {
				final ListStruct formsList = LispStructFactory.toProperList(forms);
				final ListStruct prognOperatorList = ConsStruct.valueOf(SpecialOperatorStruct.PROGN, formsList);
				return evalFunction.eval(prognOperatorList);
			}

			if (isLoadTopLevel(situationList) || (convertingForCompiler && isExecute(situationList))) {
				final ListStruct formsList = LispStructFactory.toProperList(forms);
				final ListStruct prognOperatorList = ConsStruct.valueOf(SpecialOperatorStruct.PROGN, formsList);
				return evalFunction.eval(prognOperatorList);
			}
		}

		if (isExecute(situationList)) {
			final ListStruct formsList = LispStructFactory.toProperList(forms);
			final ListStruct prognOperatorList = ConsStruct.valueOf(SpecialOperatorStruct.PROGN, formsList);
			return evalFunction.eval(prognOperatorList);
		}

		return NILStruct.INSTANCE;
	}

	private static boolean isCompileTopLevel(final ListStruct situationList) {
		return situationList.stream()
		                    .anyMatch(element -> CommonLispSymbols.COMPILE_TOPLEVEL.equals(element) || CommonLispSymbols.COMPILE.equals(element));
	}

	private static boolean isLoadTopLevel(final ListStruct situationList) {
		return situationList.stream()
		                    .anyMatch(element -> CommonLispSymbols.LOAD_TOPLEVEL.equals(element) || CommonLispSymbols.LOAD.equals(element));
	}

	private static boolean isExecute(final ListStruct situationList) {
		return situationList.stream()
		                    .anyMatch(element -> CommonLispSymbols.EXECUTE.equals(element) || CommonLispSymbols.EVAL.equals(element));
	}
}
