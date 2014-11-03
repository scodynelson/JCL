package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.structs.conditions.exceptions.ProgramErrorException;
import jcl.structs.lists.ListStruct;
import jcl.structs.lists.NullStruct;
import jcl.structs.symbols.SpecialOperator;
import jcl.structs.symbols.SymbolStruct;

import java.util.ArrayList;
import java.util.List;

public class ReturnFromAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final ReturnFromAnalyzer INSTANCE = new ReturnFromAnalyzer();

	@Override
	public ListStruct analyze(final ListStruct input, final SemanticAnalyzer analyzer) {

		if ((input.size() < 2) || (input.size() > 3)) {
			throw new ProgramErrorException("RETURN-FROM: Incorrect number of arguments: " + input.size() + ". Expected either 2 or 3 arguments.");
		}

		final LispStruct second = input.getRest().getFirst();
		if (!(second instanceof SymbolStruct)) {
			throw new ProgramErrorException("RETURN-FROM: Label must be of type SymbolStruct. Got: " + second);
		}

		if (analyzer.getBlockStack().search(second) == -1) {
			throw new ProgramErrorException("RETURN-FROM: No BLOCK with Label " + second + " is visible.");
		}

		final List<LispStruct> returnFromResultList = new ArrayList<>();
		returnFromResultList.add(SpecialOperator.RETURN_FROM);
		returnFromResultList.add(second);

		final LispStruct third = input.getRest().getRest().getFirst();
		if (!third.equals(NullStruct.INSTANCE)) {
			final LispStruct returnFromResult = analyzer.analyzeForm(third);
			returnFromResultList.add(returnFromResult);
		}

		return ListStruct.buildProperList(returnFromResultList);
	}
}
