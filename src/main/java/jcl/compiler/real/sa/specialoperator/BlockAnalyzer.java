package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.AnalysisBuilder;
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
public class BlockAnalyzer implements SpecialOperatorAnalyzer {

	@Autowired
	private BodyAnalyzer bodyAnalyzer;

	@Override
	public ListStruct analyze(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder) {

		if (input.size() < 2) {
			throw new ProgramErrorException("BLOCK: Incorrect number of arguments: " + input.size() + ". Expected at least 2 arguments.");
		}

		final LispStruct second = input.getRest().getFirst();
		if (!(second instanceof SymbolStruct)) {
			throw new ProgramErrorException("BLOCK: Label must be of type SymbolStruct. Got: " + second);
		}

		final SymbolStruct<?> label = (SymbolStruct) second;
		analysisBuilder.getBlockStack().push(label);

		try {
			final List<LispStruct> blockResultList = new ArrayList<>();
			blockResultList.add(SpecialOperator.BLOCK);
			blockResultList.add(second);

			final ListStruct body = input.getRest().getRest();
			final BodyProcessingResult bodyProcessingResult = bodyAnalyzer.analyze(analyzer, body, analysisBuilder);
			blockResultList.addAll(bodyProcessingResult.getBodyForms());

			return ListStruct.buildProperList(blockResultList);
		} finally {
			analysisBuilder.getBlockStack().pop();
		}
	}
}
