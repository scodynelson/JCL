package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.element.SetqElement;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;

@Component
public class SetqAnalyzer implements SpecialOperatorAnalyzer {

	@Override
	public SetqElement analyze(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder) {

		final ListStruct forms = input.getRest();

		final int numberOfForms = forms.size();
		if ((numberOfForms % 2) != 0) {
			throw new ProgramErrorException("SETQ: Odd number of arguments received: " + input + ". Expected an even number of arguments.");
		}

		final List<LispStruct> formsJavaList = forms.getAsJavaList();

		final List<SetqElement.SetqPair> setqPairs = new ArrayList<>(numberOfForms / 2);

		for (int index = 0; index < formsJavaList.size(); index += 2) {

			final LispStruct var = formsJavaList.get(index);
			if (!(var instanceof SymbolStruct)) {
				throw new ProgramErrorException("SETQ: Variable must be of type SymbolStruct. Got: " + var);
			}
			final SymbolStruct<?> varSymbol = (SymbolStruct) var;

			final LispStruct form = formsJavaList.get(index + 1);
			final LispStruct formAnalyzed = analyzer.analyzeForm(form, analysisBuilder);

			final SetqElement.SetqPair setqPair = new SetqElement.SetqPair(varSymbol, formAnalyzed);
			setqPairs.add(setqPair);
		}

		return new SetqElement(setqPairs);
	}
}
