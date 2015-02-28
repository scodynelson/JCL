package jcl.compiler.real.sa.analyzer.specialoperator;

import jcl.compiler.real.element.ConsElement;
import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.ListElement;
import jcl.compiler.real.element.SimpleElement;
import jcl.compiler.real.element.SymbolElement;
import jcl.compiler.real.element.specialoperator.SymbolMacroletElement;
import jcl.compiler.real.element.specialoperator.declare.DeclareElement;
import jcl.compiler.real.element.specialoperator.declare.SpecialDeclarationElement;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.EnvironmentStack;
import jcl.compiler.real.environment.Environments;
import jcl.compiler.real.environment.LambdaEnvironment;
import jcl.compiler.real.environment.SymbolMacroletEnvironment;
import jcl.compiler.real.environment.allocation.ParameterAllocation;
import jcl.compiler.real.environment.binding.EnvironmentParameterBinding;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.analyzer.specialoperator.body.BodyProcessingResult;
import jcl.compiler.real.sa.analyzer.specialoperator.body.BodyWithDeclaresAnalyzer;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.system.EnhancedLinkedList;
import jcl.types.T;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.stream.Collectors;

@Component
public class SymbolMacroletAnalyzer implements SpecialOperatorAnalyzer {

	private static final long serialVersionUID = 3878455475225336840L;

	@Autowired
	private BodyWithDeclaresAnalyzer bodyWithDeclaresAnalyzer;

	@Override
	public SymbolMacroletElement analyze(final ConsElement input, final AnalysisBuilder analysisBuilder) {

		final EnhancedLinkedList<SimpleElement> elements = input.getElements();

		final int inputSize = elements.size();
		if (inputSize < 2) {
			throw new ProgramErrorException("SYMBOL-MACROLET: Incorrect number of arguments: " + inputSize + ". Expected at least 2 arguments.");
		}

		final EnhancedLinkedList<SimpleElement> inputRest = elements.getAllButFirst();

		final SimpleElement second = inputRest.getFirst();
		if (!(second instanceof ListElement)) {
			throw new ProgramErrorException("SYMBOL-MACROLET: Parameter list must be of type ListStruct. Got: " + second);
		}

		final EnvironmentStack environmentStack = analysisBuilder.getEnvironmentStack();
		final Environment parentEnvironment = environmentStack.peek();

		final int tempClosureDepth = analysisBuilder.getClosureDepth();
		final int newClosureDepth = tempClosureDepth + 1;

		final SymbolMacroletEnvironment symbolMacroletEnvironment = new SymbolMacroletEnvironment(parentEnvironment, newClosureDepth);
		environmentStack.push(symbolMacroletEnvironment);

		final int tempBindingsPosition = analysisBuilder.getBindingsPosition();
		try {
			analysisBuilder.setClosureDepth(newClosureDepth);

			final ListElement parameters = (ListElement) second;
			final EnhancedLinkedList<SimpleElement> bodyForms = inputRest.getAllButFirst();

			final BodyProcessingResult bodyProcessingResult = bodyWithDeclaresAnalyzer.analyze(bodyForms, analysisBuilder);
			final DeclareElement declareElement = bodyProcessingResult.getDeclareElement();
			validateDeclares(declareElement);

			final List<? extends SimpleElement> parametersAsJavaList = parameters.getElements();

			final SemanticAnalyzer analyzer = analysisBuilder.getAnalyzer();

			final List<SymbolMacroletElement.SymbolMacroletElementVar> symbolMacroletVars
					= parametersAsJavaList.stream()
					                      .map(e -> getSymbolMacroletElementVar(e, declareElement, analyzer, analysisBuilder, symbolMacroletEnvironment, environmentStack))
					                      .collect(Collectors.toList());

			final List<SimpleElement> realBodyForms = bodyProcessingResult.getBodyForms();

			final List<Element> analyzedBodyForms
					= realBodyForms.stream()
					               .map(e -> analyzer.analyzeForm(e, analysisBuilder))
					               .collect(Collectors.toList());

			return new SymbolMacroletElement(symbolMacroletVars, analyzedBodyForms, symbolMacroletEnvironment);
		} finally {
			analysisBuilder.setClosureDepth(tempClosureDepth);
			analysisBuilder.setBindingsPosition(tempBindingsPosition);
			environmentStack.pop();
		}
	}

	private static void validateDeclares(final DeclareElement declareElement) {
		if (declareElement != null) {
			final List<SpecialDeclarationElement> specialDeclarationElements = declareElement.getSpecialDeclarationElements();
			if (!specialDeclarationElements.isEmpty()) {
				throw new ProgramErrorException("SYMBOL-MACROLET: Special declarations not allowed. Got: " + specialDeclarationElements);
			}
		}
	}

	private static SymbolMacroletElement.SymbolMacroletElementVar getSymbolMacroletElementVar(final SimpleElement parameter,
	                                                                                          final DeclareElement declareElement,
	                                                                                          final SemanticAnalyzer analyzer,
	                                                                                          final AnalysisBuilder analysisBuilder,
	                                                                                          final SymbolMacroletEnvironment symbolMacroletEnvironment,
	                                                                                          final EnvironmentStack environmentStack) {

		if (!(parameter instanceof ListElement)) {
			throw new ProgramErrorException("SYMBOL-MACROLET: Parameter must be of type ListStruct. Got: " + parameter);
		}

		final ListElement listParameter = (ListElement) parameter;
		final SymbolElement var = getSymbolMacroletParameterVar(listParameter, environmentStack);
		final Element expansion = getSymbolMacroletParameterExpansion(listParameter, analyzer, analysisBuilder, environmentStack);

		final LambdaEnvironment currentLambda = Environments.getEnclosingLambda(symbolMacroletEnvironment);
		final int newBindingsPosition = currentLambda.getNextParameterNumber();
		analysisBuilder.setBindingsPosition(newBindingsPosition);

		final ParameterAllocation allocation = new ParameterAllocation(newBindingsPosition);
		final EnvironmentParameterBinding binding = new EnvironmentParameterBinding(var, allocation, T.INSTANCE, expansion);
		symbolMacroletEnvironment.addLexicalBinding(binding);

		return new SymbolMacroletElement.SymbolMacroletElementVar(var, expansion);
	}

	private static SymbolElement getSymbolMacroletParameterVar(final ListElement listParameter,
	                                                           final EnvironmentStack lexicalEnvironmentStack) {

		final List<? extends SimpleElement> listParameterElement = listParameter.getElements();

		final int listParameterSize = listParameterElement.size();
		if (listParameterSize != 2) {
			throw new ProgramErrorException("SYMBOL-MACROLET: ListStruct parameter must have only 2 elements. Got: " + listParameter);
		}

		final SimpleElement listParameterFirst = listParameterElement.get(0); // TODO
		if (!(listParameterFirst instanceof SymbolElement)) {
			throw new ProgramErrorException("SYMBOL-MACROLET: ListStruct parameter first element value must be of type SymbolStruct. Got: " + listParameterFirst);
		}

		final SymbolElement parameterVar = (SymbolElement) listParameterFirst;

		final Environment globalEnvironment = lexicalEnvironmentStack.firstElement();
		final boolean hasGlobalBinding = globalEnvironment.hasLexicalBinding(parameterVar);
		if (hasGlobalBinding) {
			throw new ProgramErrorException("SYMBOL-MACROLET: ListStruct parameter first element symbol must not exist in the global environment.");
		}

		return parameterVar;
	}

	private static Element getSymbolMacroletParameterExpansion(final ListElement listParameter,
	                                                           final SemanticAnalyzer analyzer,
	                                                           final AnalysisBuilder analysisBuilder,
	                                                           final EnvironmentStack environmentStack) {

		final List<? extends SimpleElement> listParameterElement = listParameter.getElements();

		final SimpleElement parameterValue = listParameterElement.get(1); // TODO: rest().getFirst();

		// Evaluate in the outer environment. This is because we want to ensure we don't have references to symbols that may not exist.
		final Environment currentEnvironment = environmentStack.pop();

		try {
			return analyzer.analyzeForm(parameterValue, analysisBuilder);
		} finally {
			environmentStack.push(currentEnvironment);
		}
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
