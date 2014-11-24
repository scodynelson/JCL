package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;

import java.util.ArrayList;
import java.util.List;

public class TheAnalyzer implements Analyzer<ListStruct, ListStruct> {

	public static final TheAnalyzer INSTANCE = new TheAnalyzer();

	@Override
	public ListStruct analyze(final ListStruct input, final SemanticAnalyzer analyzer) {

		if (input.size() != 3) {
			throw new ProgramErrorException("THE: Incorrect number of arguments: " + input.size() + ". Expected 3 arguments.");
		}

		final LispStruct second = input.getRest().getFirst();
		if (!(second instanceof SymbolStruct) && !(second instanceof ListStruct)) {
			throw new ProgramErrorException("THE: Tag must be of type SymbolStruct or ListStruct. Got: " + second);
		}

		final LispStruct third = input.getRest().getRest().getFirst();
		final LispStruct thirdAnalyzed = analyzer.analyzeForm(third);

		final List<LispStruct> theResultList = new ArrayList<>(3);
		theResultList.add(SpecialOperator.THE);
		theResultList.add(second);
		theResultList.add(thirdAnalyzed);

		return ListStruct.buildProperList(theResultList);
	}
}
