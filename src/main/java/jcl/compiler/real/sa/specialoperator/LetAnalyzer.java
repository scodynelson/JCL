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
import jcl.structs.symbols.NILStruct;
import jcl.structs.symbols.SymbolStruct;

import java.util.List;
import java.util.Stack;

public class LetAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final LetAnalyzer INSTANCE = new LetAnalyzer();

	@Override
	public ListStruct analyze(final ListStruct input, final SemanticAnalyzer semanticAnalyzer) {

		if (input.size() < 2) {
			throw new ProgramErrorException("LET: Incorrect number of arguments: " + input.size() + ". Expected at least 2 arguments.");
		}

		final LispStruct second = input.getRest().getFirst();
		if (!(second instanceof ListStruct)) {
			throw new ProgramErrorException("LET: Parameter list must be of type ListStruct. Got: " + second);
		}

		final Stack<Environment> environmentStack = semanticAnalyzer.getEnvironmentStack();
		final Environment parentEnvironment = environmentStack.peek();

		final Environment letEnvironment = EnvironmentAccessor.createNewEnvironment(Marker.LET);
		letEnvironment.setParent(parentEnvironment);

		environmentStack.push(letEnvironment);

		final int tempPosition = semanticAnalyzer.getBindingsPosition();
		try {
			final ListStruct parameters = (ListStruct) second;
			final List<LispStruct> parametersJavaList = parameters.getAsJavaList();

			for (final LispStruct currentParameter : parametersJavaList) {
				if (currentParameter instanceof ListStruct) {
					final ListStruct listParameter = (ListStruct) currentParameter;
					if ((listParameter.size() < 1) || (listParameter.size() > 2)) {
						throw new ProgramErrorException("LET: ListStruct parameter must have only 1 or 2 elements. Got: " + currentParameter);
					}

					final LispStruct listParameterFirst = listParameter.getFirst();
					if (!(listParameterFirst instanceof SymbolStruct)) {
						throw new ProgramErrorException("LET: ListStruct parameter first element value must be of type SymbolStruct. Got: " + listParameterFirst);
					}

					final SymbolStruct<?> parameterName = (SymbolStruct) listParameterFirst;
					final LispStruct parameterValue = listParameter.getRest().getFirst();

					// Evaluate in the outer environment. This is because we want to ensure we don't have references to symbols that may not exist.
					final Environment currentEnvironment = environmentStack.pop();

					final LispStruct parameterValueInitForm;
					try {
						parameterValueInitForm = semanticAnalyzer.analyzeForm(parameterValue);
					} finally {
						environmentStack.push(currentEnvironment);
					}

					final int newBindingsPosition = EnvironmentAccessor.getNextAvailableParameterNumber(currentEnvironment);
					semanticAnalyzer.setBindingsPosition(newBindingsPosition);

					EnvironmentAccessor.createNewLetBinding(environmentStack.peek(), parameterName, newBindingsPosition, parameterValueInitForm, false);
				} else if (currentParameter instanceof SymbolStruct) {
					final SymbolStruct<?> symbolParameter = (SymbolStruct) currentParameter;

					final int newBindingsPosition = EnvironmentAccessor.getNextAvailableParameterNumber(environmentStack.peek());
					semanticAnalyzer.setBindingsPosition(newBindingsPosition);

					EnvironmentAccessor.createNewLetBinding(environmentStack.peek(), symbolParameter, newBindingsPosition, NILStruct.INSTANCE, false);
				} else {
					throw new ProgramErrorException("LET: Parameter must be of type SymbolStruct or ListStruct. Got: " + currentParameter);
				}
			}

			final ListStruct currentBodyForms = input.getRest().getRest();
			final BodyProcessingUtil.BodyProcessingResult bodyProcessingResult = BodyProcessingUtil.processBodyWithDecls(semanticAnalyzer, currentBodyForms);

			final Environment envList = environmentStack.peek();

			final ListStruct newBodyForms = ListStruct.buildProperList(bodyProcessingResult.getBodyForms());
			return new EnvironmentListStruct(envList, bodyProcessingResult.getDeclarations(), newBodyForms);
		} finally {
			semanticAnalyzer.setBindingsPosition(tempPosition);
			environmentStack.pop();
		}
	}
}
