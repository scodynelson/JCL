package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;

@Component
public class SetqAnalyzer implements SpecialOperatorAnalyzer {

	@Override
	public ListStruct analyze(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder) {

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
			final LispStruct varNameAnalyzed = analyzer.analyzeForm(varName, analysisBuilder);
			setqResultList.add(varNameAnalyzed);

			final LispStruct varValue = formsJavaList.get(i + 1);
			final LispStruct varValueAnalyzed = analyzer.analyzeForm(varValue, analysisBuilder);
			setqResultList.add(varValueAnalyzed);
		}

		return ListStruct.buildProperList(setqResultList);
	}
}
