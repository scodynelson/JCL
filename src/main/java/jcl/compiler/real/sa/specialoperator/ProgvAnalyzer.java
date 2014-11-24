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

import java.util.ArrayList;
import java.util.List;

public class ProgvAnalyzer implements Analyzer<ListStruct, ListStruct> {

	public static final ProgvAnalyzer INSTANCE = new ProgvAnalyzer();

	@Override
	public ListStruct analyze(final ListStruct input, final SemanticAnalyzer analyzer) {

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
		final LispStruct secondAnalyzed = analyzer.analyzeForm(second);

		final LispStruct third = input.getRest().getRest().getFirst();
		if (!(third instanceof ListStruct)) {
			throw new ProgramErrorException("PROGV: Values list must be of type ListStruct. Got: " + third);
		}
		final LispStruct thirdAnalyzed = analyzer.analyzeForm(third);

		final List<LispStruct> progvResultList = new ArrayList<>();
		progvResultList.add(SpecialOperator.PROGV);
		progvResultList.add(secondAnalyzed);
		progvResultList.add(thirdAnalyzed);

		final ListStruct body = input.getRest().getRest().getRest();
		final BodyProcessingResult bodyProcessingResult = BodyAnalyzer.INSTANCE.analyze(body, analyzer);
		progvResultList.addAll(bodyProcessingResult.getBodyForms());

		return ListStruct.buildProperList(progvResultList);
	}
}
