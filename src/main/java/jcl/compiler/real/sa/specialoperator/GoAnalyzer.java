package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.numbers.IntegerStruct;
import jcl.symbols.SymbolStruct;

import java.util.ListIterator;
import java.util.Map;

public class GoAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final GoAnalyzer INSTANCE = new GoAnalyzer();

	@Override
	public LispStruct analyze(final ListStruct input) {

		if (input.size() != 2) {
			throw new RuntimeException("Wrong number of arguments to special operator Go: " + input.size());
		}

		final LispStruct second = input.getRest().getFirst();
		if (!(second instanceof SymbolStruct) && !(second instanceof IntegerStruct)) {
			throw new RuntimeException("Go with invalid tag: " + input);
		}

		SymbolStruct<?> goTagSymbol = null;

		final ListIterator<Map<LispStruct, SymbolStruct<?>>> li1 = TagbodyAnalyzer.tagbodyStack.listIterator(TagbodyAnalyzer.tagbodyStack.size());

		while (li1.hasPrevious()) {
			final Map<LispStruct, SymbolStruct<?>> previousStack = li1.previous();
			if (previousStack.containsKey(second)) {
				goTagSymbol = previousStack.get(second);
				break;
			}
		}

		if (goTagSymbol == null) {
			throw new RuntimeException("No go tag named " + second + " is currently visible.");
		}

		final LispStruct first = input.getFirst();
		return new ConsStruct(first, goTagSymbol);
	}
}
