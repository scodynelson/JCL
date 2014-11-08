package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.structs.conditions.exceptions.ProgramErrorException;
import jcl.structs.lists.ListStruct;
import jcl.structs.symbols.SpecialOperator;
import jcl.structs.symbols.SymbolStruct;

import java.util.ArrayList;
import java.util.List;

public class SetqAnalyzer implements Analyzer<ListStruct, ListStruct> {

	public static final SetqAnalyzer INSTANCE = new SetqAnalyzer();

	@Override
	public ListStruct analyze(final ListStruct input, final SemanticAnalyzer analyzer) {

		final ListStruct forms = input.getRest();

		if ((forms.size() % 2) != 0) {
			throw new ProgramErrorException("SETQ: Odd number of arguments received: " + input + ". Expected an even number of arguments.");
		}

		final List<LispStruct> setqResultList = new ArrayList<>();
		setqResultList.add(SpecialOperator.SETQ);

		final List<LispStruct> formsJavaList = forms.getAsJavaList();
		for (int i = 0; i < formsJavaList.size(); i += 2) {

			final LispStruct varName = formsJavaList.get(i);
			if (!(varName instanceof SymbolStruct)) {
				throw new ProgramErrorException("SETQ: Variable name must be of type SymbolStruct. Got: " + varName);
			}
			final LispStruct varNameAnalyzed = analyzer.analyzeForm(varName);
			setqResultList.add(varNameAnalyzed);

			final LispStruct varValue = formsJavaList.get(i + 1);
			final LispStruct varValueAnalyzed = analyzer.analyzeForm(varValue);
			setqResultList.add(varValueAnalyzed);
		}

		return ListStruct.buildProperList(setqResultList);
	}
}
