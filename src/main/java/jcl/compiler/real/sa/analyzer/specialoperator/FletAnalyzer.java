package jcl.compiler.real.sa.analyzer.specialoperator;

import jcl.compiler.real.element.ConsElement;
import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.ListElement;
import jcl.compiler.real.element.SimpleElement;
import jcl.compiler.real.element.SpecialOperatorElement;
import jcl.compiler.real.element.SymbolElement;
import jcl.compiler.real.element.specialoperator.FletElement;
import jcl.compiler.real.element.specialoperator.declare.DeclareElement;
import jcl.compiler.real.element.specialoperator.declare.SpecialDeclarationElement;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.EnvironmentStack;
import jcl.compiler.real.environment.Environments;
import jcl.compiler.real.environment.FletEnvironment;
import jcl.compiler.real.environment.LambdaEnvironment;
import jcl.compiler.real.environment.allocation.EnvironmentAllocation;
import jcl.compiler.real.environment.allocation.ParameterAllocation;
import jcl.compiler.real.environment.binding.EnvironmentEnvironmentBinding;
import jcl.compiler.real.environment.binding.EnvironmentParameterBinding;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.analyzer.expander.real.MacroFunctionExpander;
import jcl.compiler.real.sa.analyzer.specialoperator.body.BodyProcessingResult;
import jcl.compiler.real.sa.analyzer.specialoperator.body.BodyWithDeclaresAnalyzer;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.symbols.SpecialOperator;
import jcl.system.EnhancedLinkedList;
import jcl.system.StackUtils;
import jcl.types.T;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import java.util.ArrayList;
import java.util.List;
import java.util.Stack;
import java.util.stream.Collectors;

@Component
public class FletAnalyzer extends MacroFunctionExpander implements SpecialOperatorAnalyzer {

	private static final long serialVersionUID = -3183832254183452606L;

	@Autowired
	private BodyWithDeclaresAnalyzer bodyWithDeclaresAnalyzer;

	/**
	 * Initializes the block macro function and adds it to the special operator 'block'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperator.FLET.setMacroFunctionExpander(this);
	}

	@Override
	public Element expand(final ConsElement form, final AnalysisBuilder analysisBuilder) {
		return analyze(form, analysisBuilder);
	}

	@Override
	public FletElement analyze(final ConsElement input, final AnalysisBuilder analysisBuilder) {

		final EnhancedLinkedList<SimpleElement> elements = input.getElements();

		final int inputSize = elements.size();
		if (inputSize < 2) {
			throw new ProgramErrorException("FLET: Incorrect number of arguments: " + inputSize + ". Expected at least 2 arguments.");
		}

		final EnhancedLinkedList<SimpleElement> inputRest = elements.getAllButFirst();

		final SimpleElement second = inputRest.getFirst();
		if (!(second instanceof ListElement)) {
			throw new ProgramErrorException("FLET: Parameter list must be of type List. Got: " + second);
		}

		final EnvironmentStack environmentStack = analysisBuilder.getEnvironmentStack();
		final Environment parentEnvironment = environmentStack.peek();

		final int tempClosureDepth = analysisBuilder.getClosureDepth();
		final int newClosureDepth = tempClosureDepth + 1;

		final FletEnvironment fletEnvironment = new FletEnvironment(parentEnvironment, newClosureDepth);
		environmentStack.push(fletEnvironment);

		final Stack<SymbolElement> functionNameStack = analysisBuilder.getFunctionNameStack();
		List<SymbolElement> functionNames = null;

		final int tempBindingsPosition = analysisBuilder.getBindingsPosition();
		try {
			analysisBuilder.setClosureDepth(newClosureDepth);

			final ListElement innerFunctions = (ListElement) second;
			final List<? extends SimpleElement> innerFunctionsJavaList = innerFunctions.getElements();
			functionNames = getFunctionNames(innerFunctionsJavaList);

			final EnhancedLinkedList<SimpleElement> bodyForms = inputRest.getAllButFirst();

			final BodyProcessingResult bodyProcessingResult = bodyWithDeclaresAnalyzer.analyze(bodyForms, analysisBuilder);
			final DeclareElement declareElement = bodyProcessingResult.getDeclareElement();

			final SemanticAnalyzer analyzer = analysisBuilder.getAnalyzer();

			final List<FletElement.FletVar> fletVars
					= innerFunctionsJavaList.stream()
					                        .map(e -> getFletVar(e, declareElement, analyzer, analysisBuilder, fletEnvironment, environmentStack))
					                        .collect(Collectors.toList());

			final List<SpecialDeclarationElement> specialDeclarationElements = declareElement.getSpecialDeclarationElements();
			specialDeclarationElements.forEach(e -> addDynamicVariableBinding(e, analysisBuilder, fletEnvironment));

			// Add function names AFTER analyzing the functions
			StackUtils.pushAll(functionNameStack, functionNames);

			final List<SimpleElement> realBodyForms = bodyProcessingResult.getBodyForms();

			final List<Element> analyzedBodyForms
					= realBodyForms.stream()
					               .map(e -> analyzer.analyzeForm(e, analysisBuilder))
					               .collect(Collectors.toList());

			return new FletElement(fletVars, analyzedBodyForms, fletEnvironment);
		} finally {
			if (functionNames != null) {
				StackUtils.popX(functionNameStack, functionNames.size());
			}

			analysisBuilder.setClosureDepth(tempClosureDepth);
			analysisBuilder.setBindingsPosition(tempBindingsPosition);
			environmentStack.pop();
		}
	}

	private List<SymbolElement> getFunctionNames(final List<? extends SimpleElement> functionDefinitions) {

		final List<SymbolElement> functionNames = new ArrayList<>(functionDefinitions.size());

		for (final SimpleElement currentFunctionDef : functionDefinitions) {
			if (!(currentFunctionDef instanceof ListElement)) {
				throw new ProgramErrorException("FLET: Function parameter must be of type List. Got: " + currentFunctionDef);
			}
			final ListElement functionList = (ListElement) currentFunctionDef;

			final SymbolElement functionName = getFunctionListParameterName(functionList);
			functionNames.add(functionName);
		}

		return functionNames;
	}

	private SymbolElement getFunctionListParameterName(final ListElement functionListParameter) {
		final SimpleElement functionListParameterFirst = functionListParameter.getElements().get(0); // TODO: should we be checking the size here???
		if (!(functionListParameterFirst instanceof SymbolElement)) {
			throw new ProgramErrorException("FLET: Function parameter first element value must be of type Symbol. Got: " + functionListParameterFirst);
		}
		return (SymbolElement) functionListParameterFirst;
	}

	private FletElement.FletVar getFletVar(final SimpleElement functionParameter,
	                                       final DeclareElement declareElement,
	                                       final SemanticAnalyzer analyzer,
	                                       final AnalysisBuilder analysisBuilder,
	                                       final FletEnvironment fletEnvironment,
	                                       final EnvironmentStack environmentStack) {

		if (!(functionParameter instanceof ConsElement)) {
			throw new ProgramErrorException("FLET: Function parameter must be of type Cons. Got: " + functionParameter);
		}

		final ConsElement functionListParameter = (ConsElement) functionParameter;
		final SymbolElement functionName = getFunctionListParameterName(functionListParameter);
		final Element functionInitForm = getFunctionParameterInitForm(functionListParameter, analyzer, analysisBuilder, environmentStack);

		final LambdaEnvironment currentLambda = Environments.getEnclosingLambda(fletEnvironment);
		final int newBindingsPosition = currentLambda.getNextParameterNumber();
		analysisBuilder.setBindingsPosition(newBindingsPosition);

		final boolean isSpecial = isSpecial(declareElement, functionName);

		final ParameterAllocation allocation = new ParameterAllocation(newBindingsPosition);
		final EnvironmentParameterBinding binding = new EnvironmentParameterBinding(functionName, allocation, T.INSTANCE, functionInitForm);
		if (isSpecial) {
			fletEnvironment.addDynamicBinding(binding);
		} else {
			fletEnvironment.addLexicalBinding(binding);
		}

		return new FletElement.FletVar(functionName, functionInitForm);
	}

	private static Element getFunctionParameterInitForm(final ConsElement functionListParameter,
	                                                    final SemanticAnalyzer analyzer,
	                                                    final AnalysisBuilder analysisBuilder,
	                                                    final EnvironmentStack environmentStack) {

		final EnhancedLinkedList<SimpleElement> functionListParameterElements = functionListParameter.getElements();

		final int functionListParameterSize = functionListParameterElements.size();
		if (functionListParameterSize < 2) {
			throw new ProgramErrorException("FLET: Incorrect number of arguments to function parameter: " + functionListParameterSize + ". Expected at least 2 arguments.");
		}

		final SimpleElement functionName = functionListParameterElements.getFirst();

		final EnhancedLinkedList<SimpleElement> functionListParameterRest = functionListParameterElements.getAllButFirst();

		final SimpleElement lambdaList = functionListParameterRest.getFirst();

		final EnhancedLinkedList<SimpleElement> body = functionListParameterRest.getAllButFirst();

		final EnhancedLinkedList<SimpleElement> innerBlock = new EnhancedLinkedList<>();
		innerBlock.add(SpecialOperatorElement.BLOCK);
		innerBlock.add(functionName);
		innerBlock.addAll(body);

		final ConsElement innerBlockListStruct = new ConsElement(innerBlock);

		final EnhancedLinkedList<SimpleElement> innerLambda = new EnhancedLinkedList<>();
		innerLambda.add(SpecialOperatorElement.LAMBDA);
		innerLambda.add(lambdaList);
		innerLambda.add(innerBlockListStruct);

		final ConsElement innerLambdaListStruct = new ConsElement(innerLambda);

		final EnhancedLinkedList<SimpleElement> innerFunction = new EnhancedLinkedList<>();
		innerFunction.add(SpecialOperatorElement.FUNCTION);
		innerFunction.add(innerLambdaListStruct);

		final ConsElement innerFunctionListStruct = new ConsElement(innerFunction);

		// Evaluate in the outer environment. This is one of the differences between Flet and Labels.
		final Environment currentEnvironment = environmentStack.pop();

		final Element functionInitForm;
		try {
			functionInitForm = analyzer.analyzeForm(innerFunctionListStruct, analysisBuilder);
		} finally {
			environmentStack.push(currentEnvironment);
		}
		return functionInitForm;
	}

	private static boolean isSpecial(final DeclareElement declareElement, final SymbolElement var) {
		boolean isSpecial = false;

		final List<SpecialDeclarationElement> specialDeclarationElements = declareElement.getSpecialDeclarationElements();
		for (final SpecialDeclarationElement specialDeclarationElement : specialDeclarationElements) {
			final SymbolElement specialVar = specialDeclarationElement.getVar();
			if (var.equals(specialVar)) {
				isSpecial = true;
				break;
			}
		}

		return isSpecial;
	}

	private void addDynamicVariableBinding(final SpecialDeclarationElement specialDeclarationElement,
	                                       final AnalysisBuilder analysisBuilder,
	                                       final FletEnvironment fletEnvironment) {

		final LambdaEnvironment currentLambda = Environments.getEnclosingLambda(fletEnvironment);
		final int newBindingsPosition = currentLambda.getNextParameterNumber();
		analysisBuilder.setBindingsPosition(newBindingsPosition);

		final SymbolElement var = specialDeclarationElement.getVar();

		final Environment bindingEnvironment = Environments.getDynamicBindingEnvironment(fletEnvironment, var);
		final EnvironmentAllocation allocation = new EnvironmentAllocation(bindingEnvironment);

		final EnvironmentEnvironmentBinding binding = new EnvironmentEnvironmentBinding(var, allocation, T.INSTANCE, bindingEnvironment);
		fletEnvironment.addDynamicBinding(binding);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
