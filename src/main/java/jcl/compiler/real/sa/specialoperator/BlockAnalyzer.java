package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.specialoperator.body.BodyAnalyzer;
import jcl.compiler.real.sa.specialoperator.body.BodyProcessingResult;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;

@Component
public class BlockAnalyzer implements Analyzer<ListStruct, ListStruct> {

	@Autowired
	private BodyAnalyzer bodyAnalyzer;

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
			final BodyProcessingResult bodyProcessingResult = bodyAnalyzer.analyze(body, analyzer);
			blockResultList.addAll(bodyProcessingResult.getBodyForms());

			return ListStruct.buildProperList(blockResultList);
		} finally {
			analyzer.getBlockStack().pop();
		}
	}
}
