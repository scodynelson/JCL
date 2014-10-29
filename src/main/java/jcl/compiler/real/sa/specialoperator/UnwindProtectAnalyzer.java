package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.structs.conditions.exceptions.ProgramErrorException;
import jcl.structs.lists.ListStruct;
import jcl.structs.symbols.SpecialOperator;

import java.util.ArrayList;
import java.util.List;

public class UnwindProtectAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final UnwindProtectAnalyzer INSTANCE = new UnwindProtectAnalyzer();

	@Override
	public LispStruct analyze(final ListStruct input) {

		if (input.size() < 2) {
			throw new ProgramErrorException("UNWIND-PROTECT: Incorrect number of arguments: " + input.size() + ". Expected at least 2 arguments.");
		}

		final LispStruct protectedForm = input.getRest().getFirst();
		final LispStruct protectedFormAnalyzed = SemanticAnalyzer.saMainLoop(protectedForm);

		final List<LispStruct> unwindProtectResultList = new ArrayList<>();
		unwindProtectResultList.add(SpecialOperator.UNWIND_PROTECT);
		unwindProtectResultList.add(protectedFormAnalyzed);

		final ListStruct cleanupForms = input.getRest().getRest();
		final List<LispStruct> cleanupFormsJavaList = cleanupForms.getAsJavaList();
		for (final LispStruct cleanupForm : cleanupFormsJavaList) {
			final LispStruct saResult = SemanticAnalyzer.saMainLoop(cleanupForm);
			unwindProtectResultList.add(saResult);
		}

		return ListStruct.buildProperList(unwindProtectResultList);
	}
}
