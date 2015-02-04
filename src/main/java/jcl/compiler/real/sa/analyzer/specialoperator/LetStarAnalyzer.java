package jcl.compiler.real.sa.analyzer.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.element.specialoperator.LetElement;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;
import java.util.ListIterator;

@Component
public class LetStarAnalyzer implements SpecialOperatorAnalyzer {

	private static final long serialVersionUID = 6456555635583825339L;

	@Autowired
	private LetAnalyzer letAnalyzer;

	@Override
	public LetElement analyze(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder) {

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

		return letAnalyzer.analyze(analyzer, body, analysisBuilder);
	}
}
