package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.old.EnvironmentAccessor;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.Marker;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.EnvironmentListStruct;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.structs.conditions.exceptions.ProgramErrorException;
import jcl.structs.lists.ListStruct;
import jcl.structs.symbols.SpecialOperator;
import jcl.structs.symbols.SymbolStruct;

import java.util.ArrayList;
import java.util.List;

public class LabelsAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final LabelsAnalyzer INSTANCE = new LabelsAnalyzer();

	@Override
	public LispStruct analyze(final ListStruct input) {

		if (input.size() < 2) {
			throw new ProgramErrorException("LABELS: Incorrect number of arguments: " + input.size() + ". Expected at least 2 arguments.");
		}

		final LispStruct second = input.getRest().getFirst();
		if (!(second instanceof ListStruct)) {
			throw new ProgramErrorException("LABELS: Parameter list must be of type ListStruct. Got: " + second);
		}

		final Environment parentEnvironment = SemanticAnalyzer.environmentStack.peek();

		final Environment labelsEnvironment = EnvironmentAccessor.createNewEnvironment(Marker.LABELS);
		labelsEnvironment.setParent(parentEnvironment);

		SemanticAnalyzer.environmentStack.push(labelsEnvironment);

		// NOTE: Prior functions that resolve later functions will be handled automatically. Unknown function calls will
		//       still be stored in the SemanticAnalyzer.undefinedFunctions field.

		final int tempPosition = SemanticAnalyzer.bindingsPosition;
		try {
			final ListStruct labelsFunctions = input.getRest();
			final List<LispStruct> labelsFunctionsJavaList = labelsFunctions.getAsJavaList();

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

				// Evaluate in the current environment. This is one of the differences between Flet and Labels.
				final Environment currentEnvironment = SemanticAnalyzer.environmentStack.peek();

				// Push the current functionName onto the Stack. This is another one of the differences between Flet and Labels.
				final LispStruct paramValueInitForm;
				try {
					SemanticAnalyzer.functionNameStack.push(functionName);
					paramValueInitForm = SemanticAnalyzer.saMainLoop(innerFunctionListStruct);
				} finally {
					SemanticAnalyzer.functionNameStack.pop();
				}

				SemanticAnalyzer.bindingsPosition = EnvironmentAccessor.getNextAvailableParameterNumber(currentEnvironment);

				EnvironmentAccessor.createNewLetBinding(currentEnvironment, functionName, SemanticAnalyzer.bindingsPosition, paramValueInitForm, false);
			}

			final ListStruct currentBodyForms = input.getRest().getRest();
			final BodyProcessingUtil.BodyProcessingResult bodyProcessingResult = BodyProcessingUtil.processBodyWithDecls(currentBodyForms);

			final Environment envList = SemanticAnalyzer.environmentStack.peek();

			final ListStruct newBodyForms = ListStruct.buildProperList(bodyProcessingResult.getBodyForms());
			return new EnvironmentListStruct(envList, bodyProcessingResult.getDeclarations(), newBodyForms);
		} finally {
			SemanticAnalyzer.bindingsPosition = tempPosition;
			SemanticAnalyzer.environmentStack.pop();
		}
	}
}
