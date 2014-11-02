package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.structs.conditions.exceptions.ProgramErrorException;
import jcl.structs.lists.ListStruct;
import jcl.structs.numbers.NumberStruct;
import jcl.structs.symbols.SpecialOperator;
import jcl.structs.symbols.SymbolStruct;

import java.util.ArrayList;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;

public class GoAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final GoAnalyzer INSTANCE = new GoAnalyzer();

	@Override
	public ListStruct analyze(final ListStruct input, final SemanticAnalyzer semanticAnalyzer) {

		if (input.size() != 2) {
			throw new ProgramErrorException("GO: Incorrect number of arguments: " + input.size() + ". Expected 2 arguments.");
		}

		final LispStruct second = input.getRest().getFirst();
		if (!(second instanceof SymbolStruct) && !(second instanceof NumberStruct)) {
			throw new ProgramErrorException("GO: Tag must be of type SymbolStruct or IntegerStruct. Got: " + second);
		}

		SymbolStruct<?> goTagSymbol = null;

		final ListIterator<Map<LispStruct, SymbolStruct<?>>> li1 = semanticAnalyzer.getTagbodyStack().listIterator(semanticAnalyzer.getTagbodyStack().size());

		while (li1.hasPrevious()) {
			final Map<LispStruct, SymbolStruct<?>> previousStack = li1.previous();
			if (previousStack.containsKey(second)) {
				goTagSymbol = previousStack.get(second);
				break;
			}
		}

		if (goTagSymbol == null) {
			throw new ProgramErrorException("GO: No TAGBODY with Tag " + second + " is visible.");
		}

		final List<LispStruct> goResultList = new ArrayList<>();
		goResultList.add(SpecialOperator.GO);
		goResultList.add(goTagSymbol);

		return ListStruct.buildProperList(goResultList);
	}
}
