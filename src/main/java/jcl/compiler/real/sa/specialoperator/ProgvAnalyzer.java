package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.structs.conditions.exceptions.ProgramErrorException;
import jcl.structs.lists.ListStruct;
import jcl.structs.symbols.SpecialOperator;
import jcl.structs.symbols.SymbolStruct;

import java.util.ArrayList;
import java.util.List;

public class ProgvAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final ProgvAnalyzer INSTANCE = new ProgvAnalyzer();

	@Override
	public ListStruct analyze(final ListStruct input) {

		if (input.size() < 3) {
			throw new ProgramErrorException("PROGV: Incorrect number of arguments: " + input.size() + ". Expected at least 3 arguments.");
		}

		final LispStruct second = input.getRest().getFirst();
		if (!(second instanceof ListStruct)) {
			throw new ProgramErrorException("PROGV: Symbols list must be of type ListStruct. Got: " + second);
		}
		final ListStruct secondListStruct = (ListStruct) second;
		final List<LispStruct> secondJavaList = secondListStruct.getAsJavaList();
		for (final LispStruct currentSecondElement : secondJavaList) {
			if (!(currentSecondElement instanceof SymbolStruct)) {
				throw new ProgramErrorException("PROGV: Element in symbols list must be of type SymbolStruct. Got: " + currentSecondElement);
			}
		}
		final LispStruct secondAnalyzed = SemanticAnalyzer.saMainLoop(second);

		final LispStruct third = input.getRest().getRest().getFirst();
		if (!(third instanceof ListStruct)) {
			throw new ProgramErrorException("PROGV: Values list must be of type ListStruct. Got: " + third);
		}
		final LispStruct thirdAnalyzed = SemanticAnalyzer.saMainLoop(third);

		final List<LispStruct> progvResultList = new ArrayList<>();
		progvResultList.add(SpecialOperator.PROGV);
		progvResultList.add(secondAnalyzed);
		progvResultList.add(thirdAnalyzed);

		final ListStruct provBody = input.getRest().getRest();
		final List<LispStruct> provBodyJavaList = provBody.getAsJavaList();
		for (final LispStruct bodyForm : provBodyJavaList) {
			final LispStruct saResult = SemanticAnalyzer.saMainLoop(bodyForm);
			progvResultList.add(saResult);
		}

		return ListStruct.buildProperList(progvResultList);
	}
}
