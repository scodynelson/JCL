package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.symbols.SymbolStruct;

import java.util.ArrayList;
import java.util.List;

public class ReturnFromAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final ReturnFromAnalyzer INSTANCE = new ReturnFromAnalyzer();

	@Override
	public LispStruct analyze(final ListStruct input) {

		final LispStruct second = input.getRest().getFirst();
		if (!(second instanceof SymbolStruct)) {
			throw new RuntimeException("ReturnFrom with invalid label: " + input);
		}

		if (BlockAnalyzer.blockStack.search(second) == -1) {
			throw new RuntimeException("No block labeled " + second + " is currently visible.");
		}

		final List<LispStruct> returnFromAnalysisList = new ArrayList<>();
		returnFromAnalysisList.add(input.getFirst());
		returnFromAnalysisList.add(second);

		final LispStruct third = input.getRest().getRest().getFirst();
		if (!third.equals(NullStruct.INSTANCE)) {
			final LispStruct saResult = SemanticAnalyzer.saMainLoop(third);
			returnFromAnalysisList.add(saResult);
		}

		return ListStruct.buildProperList(returnFromAnalysisList);
	}
}
