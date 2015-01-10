package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.EnvironmentAccessor;
import jcl.compiler.real.environment.Marker;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.element.InnerFunctionElement;
import jcl.compiler.real.sa.element.declaration.DeclareElement;
import jcl.compiler.real.sa.element.declaration.SpecialDeclarationElement;
import jcl.compiler.real.sa.specialoperator.body.BodyProcessingResult;
import jcl.compiler.real.sa.specialoperator.body.BodyWithDeclaresAnalyzer;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;
import jcl.system.StackUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;
import java.util.Stack;
import java.util.stream.Collectors;

@Component
abstract class InnerFunctionAnalyzer<T extends InnerFunctionElement, S extends InnerFunctionElement.InnerFunctionVar> implements SpecialOperatorAnalyzer {

	protected final String analyzerName;
	protected final Marker marker;
	protected final boolean getFunctionNamesBeforeInitForms;

	@Autowired
	private BodyWithDeclaresAnalyzer bodyWithDeclaresAnalyzer;

	protected InnerFunctionAnalyzer(final String analyzerName, final Marker marker, final boolean getFunctionNamesBeforeInitForms) {
		this.analyzerName = analyzerName;
		this.marker = marker;
		this.getFunctionNamesBeforeInitForms = getFunctionNamesBeforeInitForms;
	}

	@Override
	public T analyze(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder) {

		if (input.size() < 2) {
			throw new ProgramErrorException(analyzerName + ": Incorrect number of arguments: " + input.size() + ". Expected at least 2 arguments.");
		}

		final LispStruct second = input.getRest().getFirst();
		if (!(second instanceof ListStruct)) {
			throw new ProgramErrorException(analyzerName + ": Parameter list must be of type ListStruct. Got: " + second);
		}

		final Stack<Environment> environmentStack = analysisBuilder.getEnvironmentStack();
		final Environment parentEnvironment = environmentStack.peek();

		final int tempClosureDepth = analysisBuilder.getClosureDepth();
		final int newClosureDepth = tempClosureDepth + 1;

		final Environment fletEnvironment = new Environment(parentEnvironment, marker, newClosureDepth);
		environmentStack.push(fletEnvironment);

		final Stack<SymbolStruct<?>> functionNameStack = analysisBuilder.getFunctionNameStack();
		List<SymbolStruct<?>> functionNames = null;

		final int tempBindingsPosition = analysisBuilder.getBindingsPosition();
		try {
			final ListStruct innerFunctions = (ListStruct) second;
			final List<LispStruct> innerFunctionsJavaList = innerFunctions.getAsJavaList();
			functionNames = getFunctionNames(innerFunctionsJavaList);

			if (getFunctionNamesBeforeInitForms) {
				// Add function names BEFORE analyzing the functions
				StackUtils.pushAll(functionNameStack, functionNames);
			}

			final ListStruct bodyForms = input.getRest().getRest();

			final BodyProcessingResult bodyProcessingResult = bodyWithDeclaresAnalyzer.analyze(analyzer, bodyForms, analysisBuilder);
			final DeclareElement declareElement = bodyProcessingResult.getDeclareElement();

			final List<S> innerFunctionVars
					= innerFunctionsJavaList.stream()
					                        .map(e -> getInnerFunctionVar(e, declareElement, analyzer, analysisBuilder, environmentStack))
					                        .collect(Collectors.toList());

			if (!getFunctionNamesBeforeInitForms) {
				// Add function names AFTER analyzing the functions
				StackUtils.pushAll(functionNameStack, functionNames);
			}

			final List<LispStruct> realBodyForms = bodyProcessingResult.getBodyForms();

			final List<LispStruct> analyzedBodyForms
					= realBodyForms.stream()
					               .map(e -> analyzer.analyzeForm(e, analysisBuilder))
					               .collect(Collectors.toList());

			final Environment environment = environmentStack.peek();

			return getFunctionElement(innerFunctionVars, analyzedBodyForms, environment);
		} finally {
			if (functionNames != null) {
				StackUtils.popX(functionNameStack, functionNames.size());
			}

			analysisBuilder.setClosureDepth(tempClosureDepth);
			analysisBuilder.setBindingsPosition(tempBindingsPosition);
			environmentStack.pop();
		}
	}

	protected abstract T getFunctionElement(List<S> vars, List<LispStruct> bodyForms, Environment environment);

	protected abstract S getFunctionElementVar(SymbolStruct<?> var, LispStruct initForm);

	private List<SymbolStruct<?>> getFunctionNames(final List<LispStruct> functionDefList) {

		final List<SymbolStruct<?>> functionNames = new ArrayList<>(functionDefList.size());

		for (final LispStruct currentFunctionDef : functionDefList) {
			if (!(currentFunctionDef instanceof ListStruct)) {
				throw new ProgramErrorException(analyzerName + ": Function parameter must be of type ListStruct. Got: " + currentFunctionDef);
			}
			final ListStruct functionList = (ListStruct) currentFunctionDef;

			final LispStruct functionListFirst = functionList.getFirst();
			if (!(functionListFirst instanceof SymbolStruct)) {
				throw new ProgramErrorException(analyzerName + ": Function parameter first element value must be of type SymbolStruct. Got: " + functionListFirst);
			}

			final SymbolStruct<?> functionName = (SymbolStruct) functionListFirst;
			functionNames.add(functionName);
		}

		return functionNames;
	}

	private S getInnerFunctionVar(final LispStruct function,
	                              final DeclareElement declareElement,
	                              final SemanticAnalyzer analyzer,
	                              final AnalysisBuilder analysisBuilder,
	                              final Stack<Environment> environmentStack) {

		if (!(function instanceof ListStruct)) {
			throw new ProgramErrorException(analyzerName + ": Function parameter must be of type ListStruct. Got: " + function);
		}
		final ListStruct functionList = (ListStruct) function;

		final LispStruct functionListFirst = functionList.getFirst();
		if (!(functionListFirst instanceof SymbolStruct)) {
			throw new ProgramErrorException(analyzerName + ": Function parameter first element value must be of type SymbolStruct. Got: " + functionListFirst);
		}
		final SymbolStruct<?> functionName = (SymbolStruct) functionListFirst;

		final LispStruct functionListSecond = functionList.getRest().getFirst();
		if (!(functionListSecond instanceof ListStruct)) {
			throw new ProgramErrorException(analyzerName + ": Function parameter second element value must be of type ListStruct. Got: " + functionListSecond);
		}

		final ListStruct lambdaList = (ListStruct) functionListSecond;
		final ListStruct body = functionList.getRest().getRest();

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

		final LispStruct functionInitForm;
		try {
			functionInitForm = analyzer.analyzeForm(innerFunctionListStruct, analysisBuilder);
		} finally {
			environmentStack.push(currentEnvironment);
		}

		final int newBindingsPosition = EnvironmentAccessor.getNextAvailableParameterNumber(currentEnvironment);
		analysisBuilder.setBindingsPosition(newBindingsPosition);

		final boolean isSpecial = isSpecial(declareElement, functionName);

		currentEnvironment.addBinding(functionName, newBindingsPosition, functionInitForm, isSpecial);

		return getFunctionElementVar(functionName, functionInitForm);
	}

	private static boolean isSpecial(final DeclareElement declareElement, final SymbolStruct<?> var) {
		boolean isSpecial = false;

		final List<SpecialDeclarationElement> specialDeclarationElements = declareElement.getSpecialDeclarationElements();
		for (final SpecialDeclarationElement specialDeclarationElement : specialDeclarationElements) {
			final SymbolStruct<?> specialVar = specialDeclarationElement.getVar();
			if (var.equals(specialVar)) {
				isSpecial = true;
				break;
			}
		}

		return isSpecial;
	}
}
