package jcl.compiler.real.sa.analyzer.specialoperator;

import jcl.LispStruct;
import jcl.compiler.old.symbol.KeywordOld;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.analyzer.specialoperator.body.BodyAnalyzer;
import jcl.compiler.real.sa.element.Element;
import jcl.compiler.real.sa.element.NullElement;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.symbols.KeywordSymbolStruct;
import jcl.symbols.SpecialOperator;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

@Component
public class EvalWhenAnalyzer implements SpecialOperatorAnalyzer {

	private static final long serialVersionUID = -7301369273443154417L;

	@Autowired
	private BodyAnalyzer bodyAnalyzer;

	private static final Set<KeywordSymbolStruct> SITUATION_KEYWORDS = new HashSet<>(3);

	static {
		SITUATION_KEYWORDS.add(KeywordOld.CompileToplevel);
		SITUATION_KEYWORDS.add(KeywordOld.LoadToplevel);
		SITUATION_KEYWORDS.add(KeywordOld.Execute);
	}

	@Override
	public Element analyze(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder) {
		return analyze(analyzer, input, analysisBuilder, false, false);
	}

	public Element analyze(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder, final boolean isTopLevel,
	                          final boolean isCompileOrCompileFile) {

		final LispStruct second = input.getRest().getFirst();
		if (!(second instanceof ListStruct)) {
			throw new ProgramErrorException("EVAL-WHEN: Situation list must be of type ListStruct. Got: " + second);
		}

		final ListStruct situationList = (ListStruct) second;
		final List<LispStruct> situationJavaList = situationList.getAsJavaList();

		final Collection<LispStruct> difference = CollectionUtils.removeAll(situationJavaList, SITUATION_KEYWORDS);
		if (!difference.isEmpty()) {
			throw new ProgramErrorException("EVAL-WHEN: Situations must be one of ':COMPILE-TOP-LEVEL', ':LOAD-TIME-LEVEL', or ':EXECUTE'. Got: " + situationList);
		}

		final ListStruct body = input.getRest().getRest();

		if (isTopLevel) {
			if (isCompileTopLevel(situationJavaList)) {
				// (eval `(progn ,@body)))
				final ListStruct prognBody = new ConsStruct(SpecialOperator.PROGN, body);

				// TODO: what we need to do here is:
				// TODO: 1.) Get global instance of 'EVAL' function
				// TODO: 2.) Pass the new 'prognBody' to the 'EVAL' function
				// TODO: 3.) Forcefully evaluate the 'EVAL' function

			}

			if (isLoadTopLevel(situationJavaList) || (!isCompileOrCompileFile && isExecute(situationJavaList))) {
				// (funcall #'(lambda (forms) (ir1-convert-progn-body start cont forms)) body)
				bodyAnalyzer.analyze(analyzer, body, analysisBuilder);

				// TODO: what we need to do here is:
				// TODO: 1.) Create a new 'LAMBDA' function
				// TODO: 2.) Set the body of the lambda as the 'analyzedBodyForms'
				// TODO: 3.) Forcefully evaluate the created 'LAMBDA' function

			}
		} else if (isExecute(situationJavaList)) {
			// (funcall #'(lambda (forms) (ir1-convert-progn-body start cont forms)) body)
			bodyAnalyzer.analyze(analyzer, body, analysisBuilder);

			// TODO: what we need to do here is:
			// TODO: 1.) Create a new 'LAMBDA' function
			// TODO: 2.) Set the body of the lambda as the 'analyzedBodyForms'
			// TODO: 3.) Forcefully evaluate the created 'LAMBDA' function

		}

		// TODO: Really, we just do nothing. Should we actually do a 'void' return here???
		return NullElement.INSTANCE;
	}

	private static boolean isCompileTopLevel(final List<LispStruct> situationList) {
		return situationList.contains(KeywordOld.CompileToplevel);
	}

	private static boolean isLoadTopLevel(final List<LispStruct> situationList) {
		return situationList.contains(KeywordOld.LoadToplevel);
	}

	private static boolean isExecute(final List<LispStruct> situationList) {
		return situationList.contains(KeywordOld.Execute);
	}
}
