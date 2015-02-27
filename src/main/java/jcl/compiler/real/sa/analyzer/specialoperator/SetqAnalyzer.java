package jcl.compiler.real.sa.analyzer.specialoperator;

import jcl.compiler.real.element.ConsElement;
import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.SimpleElement;
import jcl.compiler.real.element.SymbolElement;
import jcl.compiler.real.element.specialoperator.SetqElement;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.system.EnhancedLinkedList;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;

@Component
public class SetqAnalyzer implements SpecialOperatorAnalyzer {

	private static final long serialVersionUID = 5324580926862048137L;

	@Override
	public SetqElement analyze(final SemanticAnalyzer analyzer, final ConsElement input, final AnalysisBuilder analysisBuilder) {

		final EnhancedLinkedList<SimpleElement> elements = input.getElements();

		final EnhancedLinkedList<SimpleElement> forms = elements.getAllButFirst();

		final int numberOfForms = forms.size();
		if ((numberOfForms % 2) != 0) {
			throw new ProgramErrorException("SETQ: Odd number of arguments received: " + input + ". Expected an even number of arguments.");
		}

		final List<SetqElement.SetqPair> setqPairs = new ArrayList<>(numberOfForms / 2);

		for (int index = 0; index < forms.size(); index += 2) {

			final SimpleElement var = forms.get(index);
			if (!(var instanceof SymbolElement)) {
				throw new ProgramErrorException("SETQ: Variable must be of type SymbolStruct. Got: " + var);
			}
			final SymbolElement varSymbol = (SymbolElement) var;

			final SimpleElement form = forms.get(index + 1);
			final Element formAnalyzed = analyzer.analyzeForm(form, analysisBuilder);

			final SetqElement.SetqPair setqPair = new SetqElement.SetqPair(varSymbol, formAnalyzed);
			setqPairs.add(setqPair);
		}

		return new SetqElement(setqPairs);
	}
}
