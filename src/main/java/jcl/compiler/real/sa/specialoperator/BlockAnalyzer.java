package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.lists.ListStruct;
import jcl.symbols.SymbolStruct;

import java.util.ArrayList;
import java.util.List;
import java.util.Stack;

public class BlockAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final BlockAnalyzer INSTANCE = new BlockAnalyzer();

	public static Stack<SymbolStruct> blockStack;

	@Override
	public LispStruct analyze(final ListStruct input) {

		final LispStruct second = input.getRest().getFirst();
		if (!(second instanceof SymbolStruct)) {
			throw new RuntimeException("Block with invalid label: " + input);
		}

		final SymbolStruct<?> label = (SymbolStruct) second;
		blockStack.push(label);

		final ListStruct listAfterBlock = input.getRest();
		final ListStruct prognResults = (ListStruct) PrognAnalyzer.INSTANCE.analyze(listAfterBlock);
		final List<LispStruct> javaPrognResults = prognResults.getAsJavaList();

		final List<LispStruct> blockAnalysisList = new ArrayList<>();

		// Rebuilding the parameter 'listStruct'
		blockAnalysisList.add(input.getFirst());
		blockAnalysisList.add(second);
		blockAnalysisList.addAll(javaPrognResults);

		blockStack.pop();
		return ListStruct.buildProperList(blockAnalysisList);
	}
}
