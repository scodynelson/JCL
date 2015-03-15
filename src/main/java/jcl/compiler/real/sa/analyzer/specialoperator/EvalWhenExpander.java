package jcl.compiler.real.sa.analyzer.specialoperator;

import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.old.symbol.KeywordOld;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.sa.FormAnalyzer;
import jcl.compiler.real.sa.analyzer.expander.MacroFunctionExpander;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.symbols.KeywordSymbolStruct;
import jcl.symbols.SpecialOperator;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class EvalWhenExpander extends MacroFunctionExpander<LispStruct> {

	private static final long serialVersionUID = -7301369273443154417L;

	private static final Set<KeywordSymbolStruct> SITUATION_KEYWORDS = new HashSet<>(3);

	static {
		SITUATION_KEYWORDS.add(KeywordOld.CompileToplevel);
		SITUATION_KEYWORDS.add(KeywordOld.LoadToplevel);
		SITUATION_KEYWORDS.add(KeywordOld.Execute);
	}

	@Autowired
	private FormAnalyzer formAnalyzer;

	/**
	 * Initializes the block macro function and adds it to the special operator 'block'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperator.EVAL_WHEN.setMacroFunctionExpander(this);
	}

	@Override
	public LispStruct expand(final ListStruct form, final Environment environment) {
		return expand(form, environment, false, false);
	}

	public LispStruct expand(final ListStruct form, final Environment environment, final boolean isTopLevel,
	                       final boolean isCompileOrCompileFile) {

		final ListStruct inputRest = form.getRest();

		final LispStruct second = inputRest.getFirst();
		if (!(second instanceof ListStruct)) {
			throw new ProgramErrorException("EVAL-WHEN: Situation list must be of type ListStruct. Got: " + second);
		}

		final ListStruct situationList = (ListStruct) second;
		final List<? extends LispStruct> situationJavaList = situationList.getAsJavaList();

		final Collection<? extends LispStruct> difference = CollectionUtils.removeAll(situationJavaList, SITUATION_KEYWORDS);
		if (!difference.isEmpty()) {
			throw new ProgramErrorException("EVAL-WHEN: Situations must be one of ':COMPILE-TOP-LEVEL', ':LOAD-TIME-LEVEL', or ':EXECUTE'. Got: " + situationList);
		}

		final List<LispStruct> forms = inputRest.getRest().getAsJavaList();

		if (isTopLevel) {
			if (isCompileTopLevel(situationJavaList)) {
				// (eval `(progn ,@body)))
//				final ListStruct prognBody = new ConsStruct(SpecialOperator.PROGN, forms);

				// TODO: what we need to do here is:
				// TODO: 1.) Get global instance of 'EVAL' function
				// TODO: 2.) Pass the new 'prognBody' to the 'EVAL' function
				// TODO: 3.) Forcefully evaluate the 'EVAL' function

			}

			if (isLoadTopLevel(situationJavaList) || (!isCompileOrCompileFile && isExecute(situationJavaList))) {
				// (funcall #'(lambda (forms) (ir1-convert-progn-body start cont forms)) body)
				final List<LispStruct> analyzedForms =
						forms.stream()
						             .map(e -> formAnalyzer.analyze(e, environment))
						             .collect(Collectors.toList());

				// TODO: what we need to do here is:
				// TODO: 1.) Create a new 'LAMBDA' function
				// TODO: 2.) Set the body of the lambda as the 'analyzedBodyForms'
				// TODO: 3.) Forcefully evaluate the created 'LAMBDA' function

			}
		} else if (isExecute(situationJavaList)) {
			// (funcall #'(lambda (forms) (ir1-convert-progn-body start cont forms)) body)
			final List<LispStruct> analyzedForms =
					forms.stream()
					             .map(e -> formAnalyzer.analyze(e, environment))
					             .collect(Collectors.toList());

			// TODO: what we need to do here is:
			// TODO: 1.) Create a new 'LAMBDA' function
			// TODO: 2.) Set the body of the lambda as the 'analyzedBodyForms'
			// TODO: 3.) Forcefully evaluate the created 'LAMBDA' function

		}

		// TODO: Really, we just do nothing. Should we actually do a 'void' return here???
		return NullStruct.INSTANCE;
	}

	private static boolean isCompileTopLevel(final List<? extends LispStruct> situationList) {
		return situationList.contains(KeywordOld.CompileToplevel);
	}

	private static boolean isLoadTopLevel(final List<? extends LispStruct> situationList) {
		return situationList.contains(KeywordOld.LoadToplevel);
	}

	private static boolean isExecute(final List<? extends LispStruct> situationList) {
		return situationList.contains(KeywordOld.Execute);
	}
}
