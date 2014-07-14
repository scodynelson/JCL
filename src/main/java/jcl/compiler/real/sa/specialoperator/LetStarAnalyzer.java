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

		if (input.size() == 1) {
			throw new RuntimeException("Wrong number of arguments to special operator LET*: " + input.size());
		}

		final LispStruct second = input.getRest().getFirst();
		if (second instanceof ListStruct) {
			final ListStruct parameters = (ListStruct) second;
			final List<LispStruct> parametersAsJavaList = parameters.getAsJavaList();

			final ListIterator<LispStruct> li = parametersAsJavaList.listIterator(parametersAsJavaList.size());

			ListStruct body = input.getRest().getRest();

			while (li.hasPrevious()) {
				final LispStruct previousParams = li.previous();

				final List<LispStruct> previousBodyJavaList = body.getAsJavaList();

				final List<LispStruct> innerLet = new ArrayList<>();
				innerLet.add(SpecialOperator.LET);
				innerLet.add(previousParams);
				innerLet.addAll(previousBodyJavaList);

				body = ListStruct.buildProperList(innerLet);
			}

			return LetAnalyzer.INSTANCE.analyze(body);
		}


		throw new RuntimeException("Improperly Formed Let*: the parameter list must be a ListStruct");
	}
}
