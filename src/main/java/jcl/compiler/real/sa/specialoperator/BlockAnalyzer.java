package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;

import java.util.ArrayList;
import java.util.List;
import java.util.Stack;

public class BlockAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final BlockAnalyzer INSTANCE = new BlockAnalyzer();

	public static final Stack<SymbolStruct<?>> BLOCK_STACK = new Stack<>();

	@Override
	public ListStruct analyze(final ListStruct input) {

		if (input.size() < 2) {
			throw new RuntimeException("BLOCK: Incorrect number of arguments: " + input.size() + ". Expected at least 2 arguments.");
		}

		final LispStruct second = input.getRest().getFirst();
		if (!(second instanceof SymbolStruct)) {
			throw new RuntimeException("BLOCK: Label must be of type SymbolStruct. Got: " + second);
		}

		final SymbolStruct<?> label = (SymbolStruct) second;
		BLOCK_STACK.push(label);

		try {
			final ListStruct blockBody = input.getRest().getRest();
			final ListStruct prognResults = PrognAnalyzer.INSTANCE.analyze(blockBody);
			final List<LispStruct> javaPrognResults = prognResults.getAsJavaList();

			final List<LispStruct> blockResultList = new ArrayList<>();
			blockResultList.add(SpecialOperator.BLOCK);
			blockResultList.add(second);
			blockResultList.addAll(javaPrognResults);

			return ListStruct.buildProperList(blockResultList);
		} finally {
			BLOCK_STACK.pop();
		}
	}
}
