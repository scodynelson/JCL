package jcl.compiler.real.sa.analyzer.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.SymbolElement;
import jcl.compiler.real.element.specialoperator.FletElement;
import jcl.compiler.real.element.specialoperator.declare.DeclareElement;
import jcl.compiler.real.element.specialoperator.declare.SpecialDeclarationElement;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.EnvironmentAccessor;
import jcl.compiler.real.environment.EnvironmentStack;
import jcl.compiler.real.environment.Environments;
import jcl.compiler.real.environment.FletEnvironment;
import jcl.compiler.real.environment.allocation.EnvironmentAllocation;
import jcl.compiler.real.environment.allocation.ParameterAllocation;
import jcl.compiler.real.environment.binding.EnvironmentEnvironmentBinding;
import jcl.compiler.real.environment.binding.EnvironmentParameterBinding;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.analyzer.specialoperator.body.BodyProcessingResult;
import jcl.compiler.real.sa.analyzer.specialoperator.body.BodyWithDeclaresAnalyzer;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;
import jcl.system.StackUtils;
import jcl.types.T;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;
import java.util.Stack;
import java.util.stream.Collectors;

@Component
public class FletAnalyzer implements SpecialOperatorAnalyzer {

	private static final long serialVersionUID = -3183832254183452606L;

	@Autowired
	private BodyWithDeclaresAnalyzer bodyWithDeclaresAnalyzer;

	@Override
	public FletElement analyze(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder) {

		if (input.size() < 2) {
			throw new ProgramErrorException("FLET: Incorrect number of arguments: " + input.size() + ". Expected at least 2 arguments.");
		}

		final LispStruct second = input.getRest().getFirst();
		if (!(second instanceof ListStruct)) {
			throw new ProgramErrorException("FLET: Parameter list must be of type ListStruct. Got: " + second);
		}

		final EnvironmentStack environmentStack = analysisBuilder.getEnvironmentStack();
		final Environment parentEnvironment = environmentStack.peek();

		final int tempClosureDepth = analysisBuilder.getClosureDepth();
		final int newClosureDepth = tempClosureDepth + 1;

		final FletEnvironment fletEnvironment = new FletEnvironment(parentEnvironment, newClosureDepth);
		environmentStack.push(fletEnvironment);

		final Stack<SymbolStruct<?>> functionNameStack = analysisBuilder.getFunctionNameStack();
		List<SymbolStruct<?>> functionNames = null;

		final int tempBindingsPosition = analysisBuilder.getBindingsPosition();
		try {
			analysisBuilder.setClosureDepth(newClosureDepth);

			final ListStruct innerFunctions = (ListStruct) second;
			final List<LispStruct> innerFunctionsJavaList = innerFunctions.getAsJavaList();
			functionNames = getFunctionNames(innerFunctionsJavaList);

			final ListStruct bodyForms = input.getRest().getRest();

			final BodyProcessingResult bodyProcessingResult = bodyWithDeclaresAnalyzer.analyze(analyzer, bodyForms, analysisBuilder);
			final DeclareElement declareElement = bodyProcessingResult.getDeclareElement();

			final List<FletElement.FletVar> fletVars
					= innerFunctionsJavaList.stream()
					                        .map(e -> getFletVar(e, declareElement, analyzer, analysisBuilder, fletEnvironment, environmentStack))
					                        .collect(Collectors.toList());

			final List<SpecialDeclarationElement> specialDeclarationElements = declareElement.getSpecialDeclarationElements();
			specialDeclarationElements.forEach(e -> addDynamicVariableBinding(e, analysisBuilder, fletEnvironment));

			// Add function names AFTER analyzing the functions
			StackUtils.pushAll(functionNameStack, functionNames);

			final List<LispStruct> realBodyForms = bodyProcessingResult.getBodyForms();

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

	private List<SymbolStruct<?>> getFunctionNames(final List<LispStruct> functionDefinitions) {

		final List<SymbolStruct<?>> functionNames = new ArrayList<>(functionDefinitions.size());

		for (final LispStruct currentFunctionDef : functionDefinitions) {
			if (!(currentFunctionDef instanceof ListStruct)) {
				throw new ProgramErrorException("FLET: Function parameter must be of type ListStruct. Got: " + currentFunctionDef);
			}
			final ListStruct functionList = (ListStruct) currentFunctionDef;

			final SymbolStruct<?> functionName = getFunctionListParameterName(functionList);
			functionNames.add(functionName);
		}

		return functionNames;
	}

	private SymbolStruct<?> getFunctionListParameterName(final ListStruct functionListParameter) {
		final LispStruct functionListParameterFirst = functionListParameter.getFirst();
		if (!(functionListParameterFirst instanceof SymbolStruct)) {
			throw new ProgramErrorException("FLET: Function parameter first element value must be of type SymbolStruct. Got: " + functionListParameterFirst);
		}
		return (SymbolStruct) functionListParameterFirst;
	}

	private FletElement.FletVar getFletVar(final LispStruct functionParameter,
	                                       final DeclareElement declareElement,
	                                       final SemanticAnalyzer analyzer,
	                                       final AnalysisBuilder analysisBuilder,
	                                       final FletEnvironment fletEnvironment,
	                                       final EnvironmentStack environmentStack) {

		if (!(functionParameter instanceof ListStruct)) {
			throw new ProgramErrorException("FLET: Function parameter must be of type ListStruct. Got: " + functionParameter);
		}

		final ListStruct functionListParameter = (ListStruct) functionParameter;
		final SymbolStruct<?> functionName = getFunctionListParameterName(functionListParameter);
		final Element functionInitForm = getFunctionParameterInitForm(functionListParameter, analyzer, analysisBuilder, environmentStack);

		final int newBindingsPosition = EnvironmentAccessor.getNextAvailableParameterNumber(fletEnvironment);
		analysisBuilder.setBindingsPosition(newBindingsPosition);

		final SymbolElement<?> functionNameSE = new SymbolElement<>(functionName);
		final boolean isSpecial = isSpecial(declareElement, functionNameSE);

		final ParameterAllocation allocation = new ParameterAllocation(newBindingsPosition);
		final EnvironmentParameterBinding binding = new EnvironmentParameterBinding(functionName, allocation, T.INSTANCE, functionInitForm);
		if (isSpecial) {
			fletEnvironment.addDynamicBinding(binding);
		} else {
			fletEnvironment.addLexicalBinding(binding);
		}

		return new FletElement.FletVar(functionNameSE, functionInitForm);
	}

	private static Element getFunctionParameterInitForm(final ListStruct functionListParameter,
	                                                    final SemanticAnalyzer analyzer,
	                                                    final AnalysisBuilder analysisBuilder,
	                                                    final EnvironmentStack environmentStack) {

		final LispStruct functionName = functionListParameter.getFirst();
		final LispStruct lambdaList = functionListParameter.getRest().getFirst();
		final ListStruct body = functionListParameter.getRest().getRest();

		final List<LispStruct> innerBlock = new ArrayList<>();
		innerBlock.add(SpecialOperator.BLOCK);
		innerBlock.add(functionName);
		innerBlock.addAll(body.getAsJavaList());

		final ListStruct innerBlockListStruct = ListStruct.buildProperList(innerBlock);

		final List<LispStruct> innerLambda = new ArrayList<>();
		innerLambda.add(SpecialOperator.LAMBDA);
		innerLambda.add(lambdaList);
		innerLambda.add(innerBlockListStruct);

		final ListStruct innerLambdaListStruct = ListStruct.buildProperList(innerLambda);

		final List<LispStruct> innerFunction = new ArrayList<>();
		innerFunction.add(SpecialOperator.FUNCTION);
		innerFunction.add(innerLambdaListStruct);

		final ListStruct innerFunctionListStruct = ListStruct.buildProperList(innerFunction);

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

	private static boolean isSpecial(final DeclareElement declareElement, final SymbolElement<?> var) {
		boolean isSpecial = false;

		final List<SpecialDeclarationElement> specialDeclarationElements = declareElement.getSpecialDeclarationElements();
		for (final SpecialDeclarationElement specialDeclarationElement : specialDeclarationElements) {
			final SymbolElement<?> specialVar = specialDeclarationElement.getVar();
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

		final int newBindingsPosition = EnvironmentAccessor.getNextAvailableParameterNumber(fletEnvironment);
		analysisBuilder.setBindingsPosition(newBindingsPosition);

		final SymbolStruct<?> var = specialDeclarationElement.getVar().getSymbolStruct();

		final Environment bindingEnvironment = Environments.getDynamicBindingEnvironment(fletEnvironment, var);
		final EnvironmentAllocation allocation = new EnvironmentAllocation(bindingEnvironment);

		final EnvironmentEnvironmentBinding binding = new EnvironmentEnvironmentBinding(var, allocation, T.INSTANCE, bindingEnvironment);
		fletEnvironment.addDynamicBinding(binding);
	}
}
