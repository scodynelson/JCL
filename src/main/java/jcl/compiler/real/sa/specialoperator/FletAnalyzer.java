package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.old.EnvironmentAccessor;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.Marker;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.FletEnvironmentListStruct;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.structs.lists.ConsStruct;
import jcl.structs.lists.ListStruct;
import jcl.structs.symbols.SpecialOperator;
import jcl.structs.symbols.SymbolStruct;

import java.util.ArrayList;
import java.util.List;

public class FletAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final FletAnalyzer INSTANCE = new FletAnalyzer();

	@Override
	public LispStruct analyze(final ListStruct input) {

		if (input.size() < 2) {
			throw new RuntimeException("FLET: Incorrect number of arguments: " + input.size() + ". Expected at least 2 arguments.");
		}

		final LispStruct second = input.getRest().getFirst();
		if (!(second instanceof ListStruct)) {
			throw new RuntimeException("FLET: Parameter list must be of type ListStruct. Got: " + second);
		}

		final Environment parentEnvironment = SemanticAnalyzer.environmentStack.peek();

		final Environment fletEnvironment = EnvironmentAccessor.createNewEnvironment(Marker.FLET);
		fletEnvironment.setParent(parentEnvironment);

		SemanticAnalyzer.environmentStack.push(fletEnvironment);

		final int tempPosition = SemanticAnalyzer.bindingsPosition;
		try {
			final ListStruct fletFunctions = input.getRest();
			final List<LispStruct> fletFunctionsJavaList = fletFunctions.getAsJavaList();

			for (final LispStruct currentFunction : fletFunctionsJavaList) {
				if (!(currentFunction instanceof ListStruct)) {
					throw new RuntimeException("FLET: Function parameter must be of type ListStruct. Got: " + currentFunction);
				}
				final ListStruct functionList = (ListStruct) currentFunction;

				final LispStruct functionListFirst = functionList.getFirst();
				if (!(functionListFirst instanceof SymbolStruct)) {
					throw new RuntimeException("FLET: Function parameter first element value must be of type SymbolStruct. Got: " + functionListFirst);
				}
				final SymbolStruct<?> functionName = (SymbolStruct) functionListFirst;

				final LispStruct functionListSecond = functionList.getRest().getFirst();
				if (!(functionListSecond instanceof ListStruct)) {
					throw new RuntimeException("FLET: Function parameter second element value must be of type ListStruct. Got: " + functionListSecond);
				}

				final ListStruct lambdaList = (ListStruct) functionListSecond;
				final ListStruct body = functionList.getRest().getRest();

				final List<LispStruct> innerBlock = new ArrayList<>();
				innerBlock.add(SpecialOperator.BLOCK);
				innerBlock.add(functionName);
				innerBlock.add(body);

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

				// Evaluate in the outer environment. This is the difference between Flet and Labels.
				final Environment currentEnvironment = SemanticAnalyzer.environmentStack.pop();
				final LispStruct paramValueInitForm = SemanticAnalyzer.saMainLoop(innerFunctionListStruct);
				SemanticAnalyzer.environmentStack.push(currentEnvironment);

				SemanticAnalyzer.bindingsPosition = EnvironmentAccessor.getNextAvailableParameterNumber(currentEnvironment);

				EnvironmentAccessor.createNewLetBinding(currentEnvironment, functionName, SemanticAnalyzer.bindingsPosition, paramValueInitForm, false);
			}

			final ListStruct body = input.getRest().getRest();

			final ListStruct prognList = new ConsStruct(SpecialOperator.PROGN, body);
			final ListStruct bodyResult = PrognAnalyzer.INSTANCE.analyze(prognList);

			final Environment envList = SemanticAnalyzer.environmentStack.peek();

			return new FletEnvironmentListStruct(envList, bodyResult);
		} finally {
			SemanticAnalyzer.bindingsPosition = tempPosition;
			SemanticAnalyzer.environmentStack.pop();
		}
	}
}
