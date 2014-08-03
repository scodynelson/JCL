package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.structs.lists.ListStruct;
import jcl.structs.symbols.SpecialOperator;
import jcl.structs.symbols.SymbolStruct;

import java.util.ArrayList;
import java.util.List;

public class SetqAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final SetqAnalyzer INSTANCE = new SetqAnalyzer();

	@Override
	public ListStruct analyze(final ListStruct input) {

		final ListStruct forms = input.getRest();

		if ((forms.size() % 2) != 0) {
			throw new RuntimeException("SETQ: Odd number of arguments received: " + input + ". Expected an even number of arguments.");
		}

		final List<LispStruct> setqResultList = new ArrayList<>();
		setqResultList.add(SpecialOperator.SETQ);

		final List<LispStruct> formsJavaList = forms.getAsJavaList();
		for (int i = 0; i < formsJavaList.size(); i += 2) {

			final LispStruct varName = formsJavaList.get(i);
			if (!(varName instanceof SymbolStruct)) {
				throw new RuntimeException("SETQ: Variable name must be of type SymbolStruct. Got: " + varName);
			}
			final LispStruct varNameAnalyzed = SemanticAnalyzer.saMainLoop(varName);
			setqResultList.add(varNameAnalyzed);

			final LispStruct varValue = formsJavaList.get(i + 1);
			final LispStruct varValueAnalyzed = SemanticAnalyzer.saMainLoop(varValue);
			setqResultList.add(varValueAnalyzed);
		}

		return ListStruct.buildProperList(setqResultList);
	}
}