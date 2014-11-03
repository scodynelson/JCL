package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.specialoperator.body.BodyAnalyzer;
import jcl.compiler.real.sa.specialoperator.body.BodyProcessingResult;
import jcl.structs.conditions.exceptions.ProgramErrorException;
import jcl.structs.lists.ListStruct;
import jcl.structs.symbols.SpecialOperator;
import jcl.structs.symbols.SymbolStruct;

import java.util.ArrayList;
import java.util.List;

public class BlockAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final BlockAnalyzer INSTANCE = new BlockAnalyzer();

	@Override
	public ListStruct analyze(final ListStruct input, final SemanticAnalyzer analyzer) {

		if (input.size() < 2) {
			throw new ProgramErrorException("BLOCK: Incorrect number of arguments: " + input.size() + ". Expected at least 2 arguments.");
		}

		final LispStruct second = input.getRest().getFirst();
		if (!(second instanceof SymbolStruct)) {
			throw new ProgramErrorException("BLOCK: Label must be of type SymbolStruct. Got: " + second);
		}

		final SymbolStruct<?> label = (SymbolStruct) second;
		analyzer.getBlockStack().push(label);

		try {
			final List<LispStruct> blockResultList = new ArrayList<>();
			blockResultList.add(SpecialOperator.BLOCK);
			blockResultList.add(second);

			final ListStruct body = input.getRest().getRest();
			final BodyProcessingResult bodyProcessingResult = BodyAnalyzer.INSTANCE.analyze(body, analyzer);
			blockResultList.addAll(bodyProcessingResult.getBodyForms());

			return ListStruct.buildProperList(blockResultList);
		} finally {
			analyzer.getBlockStack().pop();
		}
	}
}
