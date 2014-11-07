package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.EnvironmentAccessor;
import jcl.compiler.real.environment.Marker;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.EnvironmentListStruct;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.specialoperator.body.BodyProcessingResult;
import jcl.compiler.real.sa.specialoperator.body.BodyWithDeclaresAnalyzer;
import jcl.structs.conditions.exceptions.ProgramErrorException;
import jcl.structs.lists.ListStruct;
import jcl.structs.symbols.SpecialOperator;
import jcl.structs.symbols.SymbolStruct;
import jcl.system.StackUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Stack;

public class LabelsAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final LabelsAnalyzer INSTANCE = new LabelsAnalyzer();

	@Override
	public LispStruct analyze(final ListStruct input, final SemanticAnalyzer analyzer) {

		if (input.size() < 2) {
			throw new ProgramErrorException("LABELS: Incorrect number of arguments: " + input.size() + ". Expected at least 2 arguments.");
		}

		final LispStruct second = input.getRest().getFirst();
		if (!(second instanceof ListStruct)) {
			throw new ProgramErrorException("LABELS: Parameter list must be of type ListStruct. Got: " + second);
		}

		final Stack<Environment> environmentStack = analyzer.getEnvironmentStack();
		final Environment parentEnvironment = environmentStack.peek();

		final int tempClosureDepth = analyzer.getClosureDepth();
		final int newClosureDepth = tempClosureDepth + 1;

		final Environment labelsEnvironment = EnvironmentAccessor.createNewEnvironment(parentEnvironment, Marker.LABELS, newClosureDepth);
		environmentStack.push(labelsEnvironment);

		final Stack<SymbolStruct<?>> functionNameStack = analyzer.getFunctionNameStack();
		List<SymbolStruct<?>> functionNames = null;

		final int tempBindingsPosition = analyzer.getBindingsPosition();
		try {
			final ListStruct labelsFunctions = (ListStruct) second;
			final List<LispStruct> labelsFunctionsJavaList = labelsFunctions.getAsJavaList();
			functionNames = getFunctionNames(labelsFunctionsJavaList);

			// Add function names BEFORE analyzing the functions
			StackUtils.pushAll(functionNameStack, functionNames);

			for (final LispStruct currentFunction : labelsFunctionsJavaList) {
				if (!(currentFunction instanceof ListStruct)) {
					throw new ProgramErrorException("LABELS: Function parameter must be of type ListStruct. Got: " + currentFunction);
				}
				final ListStruct functionList = (ListStruct) currentFunction;

				final LispStruct functionListFirst = functionList.getFirst();
				if (!(functionListFirst instanceof SymbolStruct)) {
					throw new ProgramErrorException("LABELS: Function parameter first element value must be of type SymbolStruct. Got: " + functionListFirst);
				}
				final SymbolStruct<?> functionName = (SymbolStruct) functionListFirst;

				final LispStruct functionListSecond = functionList.getRest().getFirst();
				if (!(functionListSecond instanceof ListStruct)) {
					throw new ProgramErrorException("LABELS: Function parameter second element value must be of type ListStruct. Got: " + functionListSecond);
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

				// Evaluate in the current environment. This is one of the differences between Flet and Labels.
				final Environment currentEnvironment = environmentStack.peek();
				final LispStruct paramValueInitForm = analyzer.analyzeForm(innerFunctionListStruct);

				final int newBindingsPosition = EnvironmentAccessor.getNextAvailableParameterNumber(currentEnvironment);
				analyzer.setBindingsPosition(newBindingsPosition);

				EnvironmentAccessor.createNewEnvironmentBinding(currentEnvironment, functionName, newBindingsPosition, paramValueInitForm, false);
			}

			final ListStruct currentBodyForms = input.getRest().getRest();
			final BodyProcessingResult bodyProcessingResult = BodyWithDeclaresAnalyzer.INSTANCE.analyze(currentBodyForms, analyzer);

			final Environment envList = environmentStack.peek();

			final ListStruct newBodyForms = ListStruct.buildProperList(bodyProcessingResult.getBodyForms());
			return new EnvironmentListStruct(envList, bodyProcessingResult.getDeclarations(), newBodyForms);
		} finally {
			if (functionNames != null) {
				StackUtils.popX(functionNameStack, functionNames.size());
			}

			analyzer.setClosureDepth(tempClosureDepth);
			analyzer.setBindingsPosition(tempBindingsPosition);
			environmentStack.pop();
		}
	}

	private List<SymbolStruct<?>> getFunctionNames(final List<LispStruct> functionDefList) {

		final List<SymbolStruct<?>> functionNames = new ArrayList<>(functionDefList.size());

		for (final LispStruct currentFunctionDef : functionDefList) {
			if (!(currentFunctionDef instanceof ListStruct)) {
				throw new ProgramErrorException("LABELS: Function parameter must be of type ListStruct. Got: " + currentFunctionDef);
			}
			final ListStruct functionList = (ListStruct) currentFunctionDef;

			final LispStruct functionListFirst = functionList.getFirst();
			if (!(functionListFirst instanceof SymbolStruct)) {
				throw new ProgramErrorException("LABELS: Function parameter first element value must be of type SymbolStruct. Got: " + functionListFirst);
			}

			final SymbolStruct<?> functionName = (SymbolStruct) functionListFirst;
			functionNames.add(functionName);
		}

		return functionNames;
	}
}
