package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.old.symbol.KeywordOld;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.specialoperator.body.BodyAnalyzer;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.symbols.SpecialOperator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;

@Component
public class EvalWhenAnalyzer implements SpecialOperatorAnalyzer {

	@Autowired
	private BodyAnalyzer bodyAnalyzer;

	@Override
	public ListStruct analyze(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder) {
		return analyze(analyzer, input, analysisBuilder, false);
	}

	public ListStruct analyze(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder, final boolean isTopLevel) {

		final LispStruct second = input.getRest().getFirst();
		if (!(second instanceof ListStruct)) {
			throw new ProgramErrorException("EVAL-WHEN: Situation list must be of type ListStruct. Got: " + second);
		}

		final ListStruct situationList = (ListStruct) second;
		final List<LispStruct> situationJavaList = situationList.getAsJavaList();

		final List<LispStruct> evalWhenResultList = new ArrayList<>();
		evalWhenResultList.add(SpecialOperator.EVAL_WHEN);

		final ListStruct body = input.getRest().getRest();

		if (isTopLevel) {
			if (isCompileTopLevel(situationJavaList)) {
				final List<LispStruct> analyzedBodyForms = bodyAnalyzer.analyze(analyzer, body, analysisBuilder);
				evalWhenResultList.addAll(analyzedBodyForms);
				return ListStruct.buildProperList(evalWhenResultList);
			} else if (isLoadTopLevel(situationJavaList)) {
				// TODO: take care of processing later at load time...
				final List<LispStruct> analyzedBodyForms = bodyAnalyzer.analyze(analyzer, body, analysisBuilder);
				evalWhenResultList.addAll(analyzedBodyForms);
				return ListStruct.buildProperList(evalWhenResultList);
			} else if (isExecute(situationJavaList)) {
				// TODO: take care of processing later at execution time...
				final List<LispStruct> analyzedBodyForms = bodyAnalyzer.analyze(analyzer, body, analysisBuilder);
				evalWhenResultList.addAll(analyzedBodyForms);
				return ListStruct.buildProperList(evalWhenResultList);
			} else {
				return NullStruct.INSTANCE;
			}
		} else if (isExecute(situationJavaList)) {
			// TODO: take care of processing later at execution time...
			final List<LispStruct> analyzedBodyForms = bodyAnalyzer.analyze(analyzer, body, analysisBuilder);
			evalWhenResultList.addAll(analyzedBodyForms);
			return ListStruct.buildProperList(evalWhenResultList);
		} else {
			return NullStruct.INSTANCE;
		}
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
