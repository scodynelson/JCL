package jcl.compiler.real.sa.analyzer.specialoperator;

import jcl.compiler.real.element.ConsElement;
import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.ListElement;
import jcl.compiler.real.element.SimpleElement;
import jcl.compiler.real.element.SpecialOperatorElement;
import jcl.compiler.real.element.SymbolElement;
import jcl.compiler.real.element.specialoperator.ProgvElement;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.EnvironmentStack;
import jcl.compiler.real.environment.binding.EnvironmentParameterBinding;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.analyzer.DynamicSymbolAnalyzer;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.system.EnhancedLinkedList;
import jcl.types.T;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

@Component
public class ProgvAnalyzer implements SpecialOperatorAnalyzer {

	private static final long serialVersionUID = 2755221428467421207L;

	@Autowired
	private DynamicSymbolAnalyzer dynamicSymbolAnalyzer;

	@Override
	public ProgvElement analyze(final SemanticAnalyzer analyzer, final ConsElement input, final AnalysisBuilder analysisBuilder) {

		final EnhancedLinkedList<SimpleElement> elements = input.getElements();

		final int inputSize = elements.size();
		if (inputSize < 3) {
			throw new ProgramErrorException("PROGV: Incorrect number of arguments: " + inputSize + ". Expected at least 3 arguments.");
		}

		// Check Vars List

		final EnhancedLinkedList<SimpleElement> inputRest = elements.getAllButFirst();

		final SimpleElement second = inputRest.getFirst();
		if (!(second instanceof ConsElement)) {
			throw new ProgramErrorException("PROGV: Symbols list must be a quoted ListStruct. Got: " + second);
		}

		final ConsElement secondListStruct = (ConsElement) second;
		if (secondListStruct.getElements().size() != 2) {
			throw new ProgramErrorException("PROGV: Symbols list must be properly quoted: " + second);
		}
		if (!Objects.equals(secondListStruct.getElements().getFirst(), SpecialOperatorElement.QUOTE)) {
			throw new ProgramErrorException("PROGV: Symbols list must be quoted: " + second);
		}

		final SimpleElement actualVarsList = secondListStruct.getElements().getAllButFirst().getFirst();
		if (!(actualVarsList instanceof ListElement)) {
			throw new ProgramErrorException("PROGV: TODO: Symbols list must be of type ListStruct. Got: " + actualVarsList);
		}

		final ListElement actualVarsListStruct = (ListElement) actualVarsList;
		final List<? extends SimpleElement> actualVarsJavaList = actualVarsListStruct.getElements();
		for (final SimpleElement currentVar : actualVarsJavaList) {
			if (!(currentVar instanceof SymbolElement)) {
				throw new ProgramErrorException("PROGV: Elements in symbols list must be of type SymbolStruct. Got: " + currentVar);
			}
		}

		// Check Vals List

		final EnhancedLinkedList<SimpleElement> inputRestRest = inputRest.getAllButFirst();

		final SimpleElement third = inputRestRest.getFirst();
		if (!(third instanceof ConsElement)) {
			throw new ProgramErrorException("PROGV: Values list must be a quoted ListStruct. Got: " + third);
		}

		final ConsElement thirdListStruct = (ConsElement) third;
		if (thirdListStruct.getElements().size() != 2) {
			throw new ProgramErrorException("PROGV: Values list must be properly quoted: " + second);
		}
		if (!Objects.equals(thirdListStruct.getElements().getFirst(), SpecialOperatorElement.QUOTE)) {
			throw new ProgramErrorException("PROGV: Values list must be quoted: " + second);
		}

		final SimpleElement actualValsList = thirdListStruct.getElements().getAllButFirst().getFirst();
		if (!(actualValsList instanceof ListElement)) {
			throw new ProgramErrorException("PROGV: Values list must be of type ListStruct. Got: " + actualValsList);
		}

		final ListElement actualValsListStruct = (ListElement) actualValsList;
		final List<? extends SimpleElement> actualValsJavaList = actualValsListStruct.getElements();

		// Do other stuff

		final EnvironmentStack environmentStack = analysisBuilder.getEnvironmentStack();
		final Environment currentEnvironment = environmentStack.peek();

		final int numberOfProgvVars = actualVarsJavaList.size();
		final List<ProgvElement.ProgvVar> progvVars = new ArrayList<>(numberOfProgvVars);

		for (int i = 0; i < numberOfProgvVars; i++) {

			// NOTE: We can cast here since we checked the type earlier
			final SymbolElement var = (SymbolElement) actualVarsJavaList.get(i);
			SimpleElement val = null;
			if (i < actualValsJavaList.size()) {
				val = actualValsJavaList.get(i);
			}

			final SymbolElement varSE = dynamicSymbolAnalyzer.analyze(analyzer, var, analysisBuilder);

			final Element analyzedVal = analyzer.analyzeForm(val, analysisBuilder);
			final ProgvElement.ProgvVar progvVar = new ProgvElement.ProgvVar(varSE, analyzedVal);

			// TODO: really a 'null' allocation here???
			final EnvironmentParameterBinding binding = new EnvironmentParameterBinding(var, null, T.INSTANCE, analyzedVal);

			progvVars.add(progvVar);
		}

		final EnhancedLinkedList<SimpleElement> forms = inputRestRest.getAllButFirst();
		final List<Element> analyzedForms =
				forms.stream()
				     .map(e -> analyzer.analyzeForm(e, analysisBuilder))
				     .collect(Collectors.toList());

		return new ProgvElement(progvVars, analyzedForms, null, currentEnvironment);
	}
}
