package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.lists.ListStruct;
import jcl.numbers.NumberStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;

import java.util.ArrayList;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;

public class GoAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final GoAnalyzer INSTANCE = new GoAnalyzer();

	@Override
	public ListStruct analyze(final ListStruct input) {

		if (input.size() != 2) {
			throw new RuntimeException("GO: Incorrect number of arguments: " + input.size());
		}

		final LispStruct second = input.getRest().getFirst();
		if (!isGoTag(second)) {
			throw new RuntimeException("GO: Tag must be of type SymbolStruct or IntegerStruct. Got: " + second);
		}

		SymbolStruct<?> goTagSymbol = null;

		final ListIterator<Map<LispStruct, SymbolStruct<?>>> li1 = TagbodyAnalyzer.TAGBODY_STACK.listIterator(TagbodyAnalyzer.TAGBODY_STACK.size());

		while (li1.hasPrevious()) {
			final Map<LispStruct, SymbolStruct<?>> previousStack = li1.previous();
			if (previousStack.containsKey(second)) {
				goTagSymbol = previousStack.get(second);
				break;
			}
		}

		if (goTagSymbol == null) {
			throw new RuntimeException("GO: No TAGBODY with Tag " + second + " is visible.");
		}

		final List<LispStruct> goResultList = new ArrayList<>();
		goResultList.add(SpecialOperator.GO);
		goResultList.add(goTagSymbol);

		return ListStruct.buildProperList(goResultList);
	}

	private static boolean isGoTag(final LispStruct current) {
		return (current instanceof SymbolStruct) || (current instanceof NumberStruct);
	}
}
