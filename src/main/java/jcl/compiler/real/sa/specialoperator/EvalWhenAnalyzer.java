package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.old.symbol.KeywordOld;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.specialoperator.body.BodyAnalyzer;
import jcl.compiler.real.sa.specialoperator.body.BodyProcessingResult;
import jcl.structs.conditions.exceptions.ProgramErrorException;
import jcl.structs.lists.ListStruct;
import jcl.structs.lists.NullStruct;
import jcl.structs.symbols.KeywordSymbolStruct;
import jcl.structs.symbols.SpecialOperator;
import org.apache.commons.collections4.CollectionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class EvalWhenAnalyzer implements Analyzer<ListStruct, ListStruct> {

	private static final Logger LOGGER = LoggerFactory.getLogger(EvalWhenAnalyzer.class);

	public static final EvalWhenAnalyzer INSTANCE = new EvalWhenAnalyzer();

	private static final Set<KeywordSymbolStruct> SITUATION_KEYWORDS = new HashSet<>(3);

	static {
		SITUATION_KEYWORDS.add(KeywordOld.CompileToplevel);
		SITUATION_KEYWORDS.add(KeywordOld.LoadToplevel);
		SITUATION_KEYWORDS.add(KeywordOld.Execute);
	}

	@Override
	public ListStruct analyze(final ListStruct input, final SemanticAnalyzer analyzer) {
		return analyze(input, analyzer, false);
	}

	public static ListStruct analyze(final ListStruct input, final SemanticAnalyzer semanticAnalyzer, final boolean isTopLevel) {

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

		final List<LispStruct> evalWhenResultList = new ArrayList<>();
		evalWhenResultList.add(SpecialOperator.EVAL_WHEN);

		final ListStruct body = input.getRest().getRest();

		if (isTopLevel) {
			if (isCompileTopLevel(situationJavaList)) {
				final BodyProcessingResult bodyProcessingResult = BodyAnalyzer.INSTANCE.analyze(body, semanticAnalyzer);
				evalWhenResultList.addAll(bodyProcessingResult.getBodyForms());
				return ListStruct.buildProperList(evalWhenResultList);
			} else if (isLoadTopLevel(situationJavaList)) {
				// TODO: take care of processing later at load time...
				final BodyProcessingResult bodyProcessingResult = BodyAnalyzer.INSTANCE.analyze(body, semanticAnalyzer);
				evalWhenResultList.addAll(bodyProcessingResult.getBodyForms());
				return ListStruct.buildProperList(evalWhenResultList);
			} else if (isExecute(situationJavaList)) {
				// TODO: take care of processing later at execution time...
				final BodyProcessingResult bodyProcessingResult = BodyAnalyzer.INSTANCE.analyze(body, semanticAnalyzer);
				evalWhenResultList.addAll(bodyProcessingResult.getBodyForms());
				return ListStruct.buildProperList(evalWhenResultList);
			} else {
				// NOTE: should never get here since we did the check earlier
				LOGGER.warn("EVAL-WHEN: Unsupported situation keyword encountered: {}", situationJavaList);
				return NullStruct.INSTANCE;
			}
		} else if (isExecute(situationJavaList)) {
			// TODO: take care of processing later at execution time...
			final BodyProcessingResult bodyProcessingResult = BodyAnalyzer.INSTANCE.analyze(body, semanticAnalyzer);
			evalWhenResultList.addAll(bodyProcessingResult.getBodyForms());
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
