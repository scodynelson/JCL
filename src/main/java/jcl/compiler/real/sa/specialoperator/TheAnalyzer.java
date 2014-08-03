package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.structs.lists.ListStruct;
import jcl.structs.symbols.SpecialOperator;
import jcl.structs.symbols.SymbolStruct;

import java.util.ArrayList;
import java.util.List;

public class TheAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final TheAnalyzer INSTANCE = new TheAnalyzer();

	@Override
	public ListStruct analyze(final ListStruct input) {

		if (input.size() != 3) {
			throw new RuntimeException("THE: Incorrect number of arguments: " + input.size() + ". Expected 3 arguments.");
		}

		final LispStruct second = input.getRest().getFirst();
		if (!isTheValueType(second)) {
			throw new RuntimeException("THE: Tag must be of type SymbolStruct or ListStruct. Got: " + second);
		}
		// TODO: actually do the type comparison to verify the result of the third analyzed is an instance of the type.
		// TODO: NOTE: this probably will end up happening in the ICG as something like a 'instanceOf' check or something...

		final LispStruct third = input.getRest().getRest().getFirst();
		final LispStruct thirdAnalyzed = SemanticAnalyzer.saMainLoop(third);

		final List<LispStruct> theResultList = new ArrayList<>();
		theResultList.add(SpecialOperator.THE);
		theResultList.add(second);
		theResultList.add(thirdAnalyzed);

		return ListStruct.buildProperList(theResultList);
	}

	private static boolean isTheValueType(final LispStruct current) {
		return (current instanceof SymbolStruct) || (current instanceof ListStruct);
	}
}
