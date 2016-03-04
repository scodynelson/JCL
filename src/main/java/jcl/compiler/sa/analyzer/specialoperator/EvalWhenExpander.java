package jcl.compiler.sa.analyzer.specialoperator;

import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import jcl.LispStruct;
import jcl.compiler.CompilerVariables;
import jcl.compiler.environment.Environment;
import jcl.compiler.functions.EvalFunction;
import jcl.compiler.sa.analyzer.LispFormValueValidator;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.printer.Printer;
import jcl.symbols.NILStruct;
import jcl.symbols.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import jcl.system.CommonLispSymbols;
import org.apache.commons.collections4.CollectionUtils;
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

	@Autowired
	private LispFormValueValidator validator;

	@Autowired
	private Printer printer;

	@Override
	public SymbolStruct getFunctionSymbol() {
		return SpecialOperatorStruct.EVAL_WHEN;
	}

	@Override
	public LispStruct expand(final ListStruct form, final Environment environment) {
		validator.validateListFormSize(form, 2, "EVAL-WHEN");

		final ListStruct formRest = form.getRest();

		final LispStruct second = formRest.getFirst();
		final ListStruct situationList = validator.validateObjectType(second, "EVAL-WHEN", "SITUATION LIST", ListStruct.class);
		final List<? extends LispStruct> situationJavaList = situationList.getAsJavaList();

		final Collection<? extends LispStruct> difference = CollectionUtils.removeAll(situationJavaList, SITUATION_KEYWORDS);
		if (!difference.isEmpty()) {
			final String printedSituationList = printer.print(situationList);
			throw new ProgramErrorException("EVAL-WHEN: Situations must be one of ':COMPILE-TOPLEVEL', ':LOAD-TIMELEVEL', ':EXECUTE', 'COMPILE', 'LOAD', or 'EVAL'. Got: " + printedSituationList);
		}

		final ListStruct forms = formRest.getRest();

		final boolean isTopLevel = CompilerVariables.COMPILE_TOP_LEVEL.getVariableValue().booleanValue();
		final boolean notConvertingForInterpreter = !CompilerVariables.CONVERTING_FOR_INTERPRETER.getVariableValue().booleanValue();

		if (isTopLevel) {
			if (isCompileTopLevel(situationJavaList)) {
				final ListStruct prognOperatorList = new ConsStruct(SpecialOperatorStruct.PROGN, forms);
				return evalFunction.eval(prognOperatorList);
			}

			if (isLoadTopLevel(situationJavaList) || (notConvertingForInterpreter && isExecute(situationJavaList))) {
				final ListStruct prognOperatorList = new ConsStruct(SpecialOperatorStruct.PROGN, forms);
				return evalFunction.eval(prognOperatorList);
			}
		}

		if (isExecute(situationJavaList)) {
			final ListStruct prognOperatorList = new ConsStruct(SpecialOperatorStruct.PROGN, forms);
			return evalFunction.eval(prognOperatorList);
		}

		return NILStruct.INSTANCE;
	}

	private static boolean isCompileTopLevel(final List<? extends LispStruct> situationList) {
		return situationList.contains(CommonLispSymbols.COMPILE_TOPLEVEL)
				|| situationList.contains(CommonLispSymbols.COMPILE);
	}

	private static boolean isLoadTopLevel(final List<? extends LispStruct> situationList) {
		return situationList.contains(CommonLispSymbols.LOAD_TOPLEVEL)
				|| situationList.contains(CommonLispSymbols.LOAD);
	}

	private static boolean isExecute(final List<? extends LispStruct> situationList) {
		return situationList.contains(CommonLispSymbols.EXECUTE)
				|| situationList.contains(CommonLispSymbols.EVAL);
	}
}
