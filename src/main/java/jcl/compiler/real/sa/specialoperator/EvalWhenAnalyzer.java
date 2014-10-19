package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.old.symbol.KeywordOld;
import jcl.compiler.real.sa.Analyzer;
import jcl.structs.lists.ListStruct;
import jcl.structs.lists.NullStruct;
import jcl.structs.symbols.KeywordSymbolStruct;
import org.apache.commons.collections4.CollectionUtils;

import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class EvalWhenAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final EvalWhenAnalyzer INSTANCE = new EvalWhenAnalyzer();

	private static final Set<KeywordSymbolStruct> SITUATION_KEYWORDS = new HashSet<>(3);

	static {
		SITUATION_KEYWORDS.add(KeywordOld.CompileToplevel);
		SITUATION_KEYWORDS.add(KeywordOld.LoadToplevel);
		SITUATION_KEYWORDS.add(KeywordOld.Execute);
	}

	@Override
	public LispStruct analyze(final ListStruct input) {
		return analyze(input, false);
	}

	public ListStruct analyze(final ListStruct input, final boolean isTopLevel) {

		final LispStruct second = input.getRest().getFirst();
		if (!(second instanceof ListStruct)) {
			throw new RuntimeException("EVAL-WHEN: Situation list must be of type ListStruct. Got: " + second);
		}

		final ListStruct situationList = (ListStruct) second;
		final List<LispStruct> situationJavaList = situationList.getAsJavaList();

		final Collection<LispStruct> difference = CollectionUtils.removeAll(situationJavaList, SITUATION_KEYWORDS);
		if (!difference.isEmpty()) {
			throw new RuntimeException("EVAL-WHEN: Situations must be one of ':COMPILE-TOP-LEVEL', ':LOAD-TIME-LEVEL', or ':EXECUTE'. Got: " + situationList);
		}

		final ListStruct forms = input.getRest().getRest();
		if (isTopLevel) {
			if (isCompileTopLevel(situationJavaList)) {
				return PrognAnalyzer.INSTANCE.analyze(forms);
			} else if (isLoadTopLevel(situationJavaList)) {
				// TODO: take care of processing later at load time...
				return PrognAnalyzer.INSTANCE.analyze(forms);
			} else if (isExecute(situationJavaList)) {
				// TODO: take care of processing later at execution time...
				return PrognAnalyzer.INSTANCE.analyze(forms);
			} else {
				// NOTE: should never get here since we did the check earlier
				// TODO: add warning logger here at some point
				return NullStruct.INSTANCE;
			}
		} else if (isExecute(situationJavaList)) {
			// TODO: take care of processing later at execution time...
			return PrognAnalyzer.INSTANCE.analyze(forms);
		} else {
			return NullStruct.INSTANCE;
		}
	}

	private boolean isCompileTopLevel(final List<LispStruct> situationList) {
		return situationList.contains(KeywordOld.CompileToplevel);
	}

	private boolean isLoadTopLevel(final List<LispStruct> situationList) {
		return situationList.contains(KeywordOld.LoadToplevel);
	}

	private boolean isExecute(final List<LispStruct> situationList) {
		return situationList.contains(KeywordOld.Execute);
	}
}
