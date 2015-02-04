package jcl.compiler.real.sa.analyzer.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.environment.DynamicEnvironment;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.analyzer.DynamicSymbolStructAnalyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.element.Element;
import jcl.compiler.real.sa.element.SymbolElement;
import jcl.compiler.real.sa.element.specialoperator.ProgvElement;
import jcl.compiler.real.sa.analyzer.specialoperator.body.BodyAnalyzer;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Stack;

@Component
public class ProgvAnalyzer implements SpecialOperatorAnalyzer {

	private static final long serialVersionUID = 2755221428467421207L;

	@Autowired
	private BodyAnalyzer bodyAnalyzer;

	@Autowired
	private DynamicSymbolStructAnalyzer dynamicSymbolStructAnalyzer;

	@Override
	public ProgvElement analyze(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder) {

		final int inputSize = input.size();
		if (inputSize < 3) {
			throw new ProgramErrorException("PROGV: Incorrect number of arguments: " + inputSize + ". Expected at least 3 arguments.");
		}

		// Check Vars List

		final LispStruct second = input.getRest().getFirst();
		if (!(second instanceof ListStruct)) {
			throw new ProgramErrorException("PROGV: Symbols list must be a quoted ListStruct. Got: " + second);
		}

		final ListStruct secondListStruct = (ListStruct) second;
		if (secondListStruct.size() != 2) {
			throw new ProgramErrorException("PROGV: Symbols list must be properly quoted: " + second);
		}
		if (!Objects.equals(secondListStruct.getFirst(), SpecialOperator.QUOTE)) {
			throw new ProgramErrorException("PROGV: Symbols list must be quoted: " + second);
		}

		final LispStruct actualVarsList = secondListStruct.getRest().getFirst();
		if (!(actualVarsList instanceof ListStruct)) {
			throw new ProgramErrorException("PROGV: TODO: Symbols list must be of type ListStruct. Got: " + actualVarsList);
		}

		final ListStruct actualVarsListStruct = (ListStruct) actualVarsList;
		final List<LispStruct> actualVarsJavaList = actualVarsListStruct.getAsJavaList();
		for (final LispStruct currentVar : actualVarsJavaList) {
			if (!(currentVar instanceof SymbolStruct)) {
				throw new ProgramErrorException("PROGV: Elements in symbols list must be of type SymbolStruct. Got: " + currentVar);
			}
		}

		// Check Vals List

		final LispStruct third = input.getRest().getRest().getFirst();
		if (!(third instanceof ListStruct)) {
			throw new ProgramErrorException("PROGV: Values list must be a quoted ListStruct. Got: " + third);
		}

		final ListStruct thirdListStruct = (ListStruct) third;
		if (thirdListStruct.size() != 2) {
			throw new ProgramErrorException("PROGV: Values list must be properly quoted: " + second);
		}
		if (!Objects.equals(thirdListStruct.getFirst(), SpecialOperator.QUOTE)) {
			throw new ProgramErrorException("PROGV: Values list must be quoted: " + second);
		}

		final LispStruct actualValsList = thirdListStruct.getRest().getFirst();
		if (!(actualValsList instanceof ListStruct)) {
			throw new ProgramErrorException("PROGV: Values list must be of type ListStruct. Got: " + actualValsList);
		}

		final ListStruct actualValsListStruct = (ListStruct) actualValsList;
		final List<LispStruct> actualValsJavaList = actualValsListStruct.getAsJavaList();

		// Do other stuff

		final Stack<DynamicEnvironment> dynamicEnvironmentStack = analysisBuilder.getDynamicEnvironmentStack();
		final DynamicEnvironment parentDynamicEnvironment = dynamicEnvironmentStack.peek();

		final DynamicEnvironment progvEnvironment = new DynamicEnvironment(parentDynamicEnvironment);
		dynamicEnvironmentStack.push(progvEnvironment);

		final int tempBindingsPosition = analysisBuilder.getBindingsPosition();
		try {
			final int numberOfProgvVars = actualVarsJavaList.size();
			final List<ProgvElement.ProgvVar> progvVars = new ArrayList<>(numberOfProgvVars);

			for (int i = 0; i < numberOfProgvVars; i++) {

				// NOTE: We can cast here since we checked the type earlier
				final SymbolStruct<?> var = (SymbolStruct<?>) actualVarsJavaList.get(i);
				LispStruct val = null;
				if (i < actualValsJavaList.size()) {
					val = actualValsJavaList.get(i);
				}

				final SymbolElement<?> varSE = dynamicSymbolStructAnalyzer.analyze(analyzer, var, analysisBuilder);

				final Element analyzedVal = analyzer.analyzeForm(val, analysisBuilder);
				final ProgvElement.ProgvVar progvVar = new ProgvElement.ProgvVar(varSE, analyzedVal);

				progvEnvironment.addBinding(var, null, analyzedVal, true);

				progvVars.add(progvVar);
			}

			final ListStruct bodyForms = input.getRest().getRest().getRest();
			final List<Element> analyzedBodyForms = bodyAnalyzer.analyze(analyzer, bodyForms, analysisBuilder);

			return new ProgvElement(progvVars, analyzedBodyForms, progvEnvironment);
		} finally {
			analysisBuilder.setBindingsPosition(tempBindingsPosition);
			dynamicEnvironmentStack.pop();
		}
	}
}
