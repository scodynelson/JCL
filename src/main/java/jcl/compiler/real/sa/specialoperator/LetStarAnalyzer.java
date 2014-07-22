package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;

import java.util.ArrayList;
import java.util.List;
import java.util.ListIterator;

public class LetStarAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final LetStarAnalyzer INSTANCE = new LetStarAnalyzer();

	@Override
	public LispStruct analyze(final ListStruct input) {

		if (input.size() < 2) {
			throw new RuntimeException("LET*: Incorrect number of arguments: " + input.size() + ". Expected at least 2 arguments.");
		}

		final LispStruct second = input.getRest().getFirst();
		if (!(second instanceof ListStruct)) {
			throw new RuntimeException("LET*: Parameter list must be of type ListStruct. Got: " + second);
		}

		final ListStruct parameters = (ListStruct) second;
		final List<LispStruct> parametersAsJavaList = parameters.getAsJavaList();

		final ListIterator<LispStruct> iterator = parametersAsJavaList.listIterator(parametersAsJavaList.size());

		ListStruct body = input.getRest().getRest();

		while (iterator.hasPrevious()) {
			final LispStruct previousParams = iterator.previous();

			final List<LispStruct> previousBodyJavaList = body.getAsJavaList();

			final List<LispStruct> innerLet = new ArrayList<>();
			innerLet.add(SpecialOperator.LET);
			innerLet.add(previousParams);
			innerLet.addAll(previousBodyJavaList);

			body = ListStruct.buildProperList(innerLet);
		}

		return LetAnalyzer.INSTANCE.analyze(body);
	}
}
