package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;

import java.util.ArrayList;
import java.util.List;

public class UnwindProtectAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final UnwindProtectAnalyzer INSTANCE = new UnwindProtectAnalyzer();

	@Override
	public LispStruct analyze(final ListStruct input) {

		if (input.size() < 2) {
			throw new RuntimeException("UNWIND-PROTECT: Incorrect number of arguments: " + input.size() + ". Expected at least 2 arguments.");
		}

		final LispStruct protectedForm = input.getRest().getFirst();
		final LispStruct protectedFormAnalyzed = SemanticAnalyzer.saMainLoop(protectedForm);

		final ListStruct cleanupForms = input.getRest().getRest();
		final ListStruct prognResults = PrognAnalyzer.INSTANCE.analyze(cleanupForms);
		final List<LispStruct> javaPrognResults = prognResults.getAsJavaList();

		final List<LispStruct> unwindProtectResultList = new ArrayList<>();
		unwindProtectResultList.add(SpecialOperator.UNWIND_PROTECT);
		unwindProtectResultList.add(protectedFormAnalyzed);
		unwindProtectResultList.addAll(javaPrognResults);

		return ListStruct.buildProperList(unwindProtectResultList);
	}
}
