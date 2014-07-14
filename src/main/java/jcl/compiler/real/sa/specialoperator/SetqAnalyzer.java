package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.SymbolStructAnalyzer;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.symbols.SymbolStruct;

import java.util.ArrayList;
import java.util.List;

public class SetqAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final SetqAnalyzer INSTANCE = new SetqAnalyzer();

	@Override
	public LispStruct analyze(final ListStruct input) {
		final ListStruct setqForms = input.getRest();

		if (setqForms.equals(NullStruct.INSTANCE)) {
			return NullStruct.INSTANCE;
		}

		if ((setqForms.size() % 2) != 0) {
			throw new RuntimeException("SETQ called with odd number of arguments: " + input);
		}

		final List<LispStruct> setqAnalysisList = new ArrayList<>();
		setqAnalysisList.add(input.getFirst());

		final List<LispStruct> javaSetqForms = setqForms.getAsJavaList();
		for (int i = 0; i < javaSetqForms.size(); i += 2) {

			final LispStruct firstElement = javaSetqForms.get(i);
			if (!(firstElement instanceof SymbolStruct)) {
				throw new RuntimeException("SETQ: " + firstElement + " is not a SymbolStruct.");
			}

			final SymbolStruct<?> symbolStruct = SymbolStructAnalyzer.INSTANCE.analyze((SymbolStruct) firstElement);
			setqAnalysisList.add(symbolStruct);

			final LispStruct secondElement = javaSetqForms.get(i + 1);

			final LispStruct formResult = SemanticAnalyzer.saMainLoop(secondElement);
			setqAnalysisList.add(formResult);
		}

		return ListStruct.buildProperList(setqAnalysisList);
	}
}
