package jcl.compiler.real.sa.analyzer.specialoperator;

import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.sa.analyzer.expander.MacroFunctionExpander;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.printer.Printer;
import jcl.symbols.KeywordSymbolStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;
import jcl.system.CommonLispSymbols;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class EvalWhenExpander extends MacroFunctionExpander<LispStruct> {

	private static final long serialVersionUID = -7301369273443154417L;

	private static final Set<KeywordSymbolStruct> SITUATION_KEYWORDS = new HashSet<>(3);

	static {
		SITUATION_KEYWORDS.add(CommonLispSymbols.COMPILE_TOPLEVEL);
		SITUATION_KEYWORDS.add(CommonLispSymbols.LOAD_TOPLEVEL);
		SITUATION_KEYWORDS.add(CommonLispSymbols.EXECUTE);
	}

	@Autowired
	private Printer printer;

	/**
	 * Initializes the eval-when macro function and adds it to the special operator 'eval-when'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperator.EVAL_WHEN.setMacroFunctionExpander(this);
	}

	@Override
	public LispStruct expand(final ListStruct form, final Environment environment) {
		return expand(form, false, false);
	}

	public LispStruct expand(final ListStruct form, final boolean isTopLevel, final boolean isCompileOrCompileFile) {

		final int formSize = form.size();
		if (formSize < 2) {
			throw new ProgramErrorException("EVAL-WHEN: Incorrect number of arguments: " + formSize + ". Expected at least 2 arguments.");
		}

		final ListStruct formRest = form.getRest();

		final LispStruct second = formRest.getFirst();
		if (!(second instanceof ListStruct)) {
			final String printedObject = printer.print(second);
			throw new ProgramErrorException("EVAL-WHEN: Situation list must be a list. Got: " + printedObject);
		}

		final ListStruct situationList = (ListStruct) second;
		final List<? extends LispStruct> situationJavaList = situationList.getAsJavaList();

		final Collection<? extends LispStruct> difference = CollectionUtils.removeAll(situationJavaList, SITUATION_KEYWORDS);
		if (!difference.isEmpty()) {
			final String printedSituationList = printer.print(situationList);
			throw new ProgramErrorException("EVAL-WHEN: Situations must be one of ':COMPILE-TOP-LEVEL', ':LOAD-TIME-LEVEL', or ':EXECUTE'. Got: " + printedSituationList);
		}

		final ListStruct forms = formRest.getRest();

		if (isTopLevel) {
			if (isCompileTopLevel(situationJavaList)) {
				// (eval `(progn ,@body)))
				final ListStruct prognOperatorList = new ConsStruct(SpecialOperator.PROGN, forms);

				// TODO: what we need to do here is:
				// TODO: 1.) Get global instance of 'EVAL' function
				// TODO: 2.) Pass the new 'prognBody' to the 'EVAL' function
				// TODO: 3.) Forcefully evaluate the 'EVAL' function
			}

			if (isLoadTopLevel(situationJavaList) || (!isCompileOrCompileFile && isExecute(situationJavaList))) {
				// (eval (funcall (function (lambda (forms) (progn forms))) body))

				final SymbolStruct<?> formsSymbol = new SymbolStruct<>("FORMS");
				final ListStruct prognOperatorList = ListStruct.buildProperList(SpecialOperator.PROGN, formsSymbol);

				final ListStruct lambdaArgumentsList = ListStruct.buildProperList(formsSymbol);
				final ListStruct lambdaOperatorList = ListStruct.buildProperList(SpecialOperator.LAMBDA, lambdaArgumentsList, prognOperatorList);

				final ListStruct functionOperatorList = ListStruct.buildProperList(SpecialOperator.FUNCTION, lambdaOperatorList);
				final ListStruct funcallList = ListStruct.buildProperList(functionOperatorList, forms);

				// TODO: what we need to do here is:
				// TODO: 1.) Create a new 'LAMBDA' function
				// TODO: 2.) Set the body of the lambda as the 'analyzedBodyForms'
				// TODO: 3.) Forcefully evaluate the created 'LAMBDA' function
			}
		} else if (isExecute(situationJavaList)) {
			// (eval (funcall (function (lambda (forms) (progn forms))) body))

			final SymbolStruct<?> formsSymbol = new SymbolStruct<>("FORMS");
			final ListStruct prognOperatorList = ListStruct.buildProperList(SpecialOperator.PROGN, formsSymbol);

			final ListStruct lambdaArgumentsList = ListStruct.buildProperList(formsSymbol);
			final ListStruct lambdaOperatorList = ListStruct.buildProperList(SpecialOperator.LAMBDA, lambdaArgumentsList, prognOperatorList);

			final ListStruct functionOperatorList = ListStruct.buildProperList(SpecialOperator.FUNCTION, lambdaOperatorList);
			final ListStruct funcallList = ListStruct.buildProperList(functionOperatorList, forms);

			// TODO: what we need to do here is:
			// TODO: 1.) Create a new 'LAMBDA' function
			// TODO: 2.) Set the body of the lambda as the 'analyzedBodyForms'
			// TODO: 3.) Forcefully evaluate the created 'LAMBDA' function
		}

		return NullStruct.INSTANCE;
	}

	private static boolean isCompileTopLevel(final List<? extends LispStruct> situationList) {
		return situationList.contains(CommonLispSymbols.COMPILE_TOPLEVEL);
	}

	private static boolean isLoadTopLevel(final List<? extends LispStruct> situationList) {
		return situationList.contains(CommonLispSymbols.LOAD_TOPLEVEL);
	}

	private static boolean isExecute(final List<? extends LispStruct> situationList) {
		return situationList.contains(CommonLispSymbols.EXECUTE);
	}
}
