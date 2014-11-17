package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.EnvironmentLispStruct;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.structs.conditions.exceptions.ProgramErrorException;
import jcl.structs.lists.ListStruct;
import jcl.structs.symbols.SpecialOperator;

import java.util.ArrayList;
import java.util.List;
import java.util.ListIterator;

public class LetStarAnalyzer implements Analyzer<EnvironmentLispStruct, ListStruct> {

	public static final LetStarAnalyzer INSTANCE = new LetStarAnalyzer();

	@Override
	public EnvironmentLispStruct analyze(final ListStruct input, final SemanticAnalyzer analyzer) {

		if (input.size() < 2) {
			throw new ProgramErrorException("LET*: Incorrect number of arguments: " + input.size() + ". Expected at least 2 arguments.");
		}

		final LispStruct second = input.getRest().getFirst();
		if (!(second instanceof ListStruct)) {
			throw new ProgramErrorException("LET*: Parameter list must be of type ListStruct. Got: " + second);
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

		return LetAnalyzer.INSTANCE.analyze(body, analyzer);
	}
}
