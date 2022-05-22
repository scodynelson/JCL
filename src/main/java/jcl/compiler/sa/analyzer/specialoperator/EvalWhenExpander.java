package jcl.compiler.sa.analyzer.specialoperator;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import jcl.compiler.environment.Environment;
import jcl.compiler.function.InternalEval;
import jcl.compiler.function.expanders.MacroFunctionExpander;
import jcl.compiler.sa.FormAnalyzer;
import jcl.lang.ConsStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.ProgramErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.statics.CommonLispSymbols;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class EvalWhenExpander extends MacroFunctionExpander<LispStruct> {

	public static final EvalWhenExpander INSTANCE = new EvalWhenExpander();

	private static final Set<SymbolStruct> SITUATION_KEYWORDS = new HashSet<>(6);

	/*
	TODO: Handle the "globalness" of the following
	 */

	private Mode mode = Mode.NOT_COMPILE_TIME;

	static {
		SITUATION_KEYWORDS.add(CommonLispSymbols.COMPILE_TOPLEVEL);
		SITUATION_KEYWORDS.add(CommonLispSymbols.LOAD_TOPLEVEL);
		SITUATION_KEYWORDS.add(CommonLispSymbols.EXECUTE);

		SITUATION_KEYWORDS.add(CommonLispSymbols.COMPILE);
		SITUATION_KEYWORDS.add(CommonLispSymbols.LOAD);
		SITUATION_KEYWORDS.add(CommonLispSymbols.EVAL);
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.EVAL_WHEN;
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

		final boolean compileTopLevel = isCompileTopLevel(situationList);
		final boolean loadTopLevel = isLoadTopLevel(situationList);
		final boolean execute = isExecute(situationList);

		final Mode previousMode = mode;
		try {
			Action action = Action.DISCARD;

			if (compileTopLevel && loadTopLevel) {
				action = Action.PROCESS;
				mode = Mode.COMPILE_TIME_TOO;
			}
			if (!compileTopLevel && loadTopLevel && execute) {
				if (mode == Mode.COMPILE_TIME_TOO) {
					action = Action.PROCESS;
					mode = Mode.COMPILE_TIME_TOO;
				}
			}
			if (!compileTopLevel && loadTopLevel && execute) {
				if (mode == Mode.NOT_COMPILE_TIME) {
					action = Action.PROCESS;
					mode = Mode.NOT_COMPILE_TIME;
				}
			}
			if (!compileTopLevel && loadTopLevel && !execute) {
				action = Action.PROCESS;
				mode = Mode.NOT_COMPILE_TIME;
			}
			if (compileTopLevel && !loadTopLevel) {
				action = Action.EVALUATE;
			}
			if (!compileTopLevel && !loadTopLevel && execute) {
				if (mode == Mode.COMPILE_TIME_TOO) {
					action = Action.EVALUATE;
				}
				if (mode == Mode.NOT_COMPILE_TIME) {
					action = Action.DISCARD;
				}
			}
			if (!compileTopLevel && !loadTopLevel && !execute) {
				action = Action.DISCARD;
			}

			switch (action) {
				case PROCESS:
					final ListStruct formsList = ListStruct.toLispList(forms);
					final ListStruct prognOperatorList = ConsStruct.toLispCons(CommonLispSymbols.PROGN, formsList);
					if (mode == Mode.COMPILE_TIME_TOO) {
						final ListStruct prognOperatorListCopy = prognOperatorList.copyTree();
						InternalEval.eval(prognOperatorListCopy);
					}
					return FormAnalyzer.analyze(prognOperatorList, environment);
				case EVALUATE:
					final ListStruct formsList1 = ListStruct.toLispList(forms);
					final ListStruct prognOperatorList1 = ConsStruct.toLispCons(CommonLispSymbols.PROGN, formsList1);
					return InternalEval.eval(prognOperatorList1);
				case DISCARD:
					return NILStruct.INSTANCE;
			}

			return NILStruct.INSTANCE;
		} finally {
			mode = previousMode;
		}
	}

	private static boolean isCompileTopLevel(final ListStruct situationList) {
		return situationList.stream()
		                    .anyMatch(element -> CommonLispSymbols.COMPILE_TOPLEVEL.eq(element) || CommonLispSymbols.COMPILE.eq(element));
	}

	private static boolean isLoadTopLevel(final ListStruct situationList) {
		return situationList.stream()
		                    .anyMatch(element -> CommonLispSymbols.LOAD_TOPLEVEL.eq(element) || CommonLispSymbols.LOAD.eq(element));
	}

	private static boolean isExecute(final ListStruct situationList) {
		return situationList.stream()
		                    .anyMatch(element -> CommonLispSymbols.EXECUTE.eq(element) || CommonLispSymbols.EVAL.eq(element));
	}

	private enum Mode {
		COMPILE_TIME_TOO,
		NOT_COMPILE_TIME
	}

	private enum Action {
		PROCESS,
		EVALUATE,
		DISCARD
	}
}
