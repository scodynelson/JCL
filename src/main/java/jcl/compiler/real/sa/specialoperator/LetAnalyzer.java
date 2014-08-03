package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.old.EnvironmentAccessor;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.Marker;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.LetEnvironmentListStruct;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.structs.lists.ConsStruct;
import jcl.structs.lists.ListStruct;
import jcl.structs.symbols.NILStruct;
import jcl.structs.symbols.SpecialOperator;
import jcl.structs.symbols.SymbolStruct;

import java.util.List;

public class LetAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final LetAnalyzer INSTANCE = new LetAnalyzer();

	@Override
	public ListStruct analyze(final ListStruct input) {

		if (input.size() < 2) {
			throw new RuntimeException("LET: Incorrect number of arguments: " + input.size() + ". Expected at least 2 arguments.");
		}

		final LispStruct second = input.getRest().getFirst();
		if (!(second instanceof ListStruct)) {
			throw new RuntimeException("LET: Parameter list must be of type ListStruct. Got: " + second);
		}

		final Environment parentEnvironment = SemanticAnalyzer.environmentStack.peek();

		final Environment letEnvironment = EnvironmentAccessor.createNewEnvironment(Marker.LET);
		letEnvironment.setParent(parentEnvironment);

		SemanticAnalyzer.environmentStack.push(letEnvironment);

		final int tempPosition = SemanticAnalyzer.bindingsPosition;
		try {
			final ListStruct parameters = (ListStruct) second;
			final List<LispStruct> parametersJavaList = parameters.getAsJavaList();

			for (final LispStruct currentParameter : parametersJavaList) {
				if (currentParameter instanceof ListStruct) {
					final ListStruct listParameter = (ListStruct) currentParameter;
					if ((listParameter.size() < 1) || (listParameter.size() > 2)) {
						throw new RuntimeException("LET: ListStruct parameter must have only 1 or 2 elements. Got: " + currentParameter);
					}

					final LispStruct listParameterFirst = listParameter.getFirst();
					if (!(listParameterFirst instanceof SymbolStruct)) {
						throw new RuntimeException("LET: ListStruct parameter first element value must be of type SymbolStruct. Got: " + listParameterFirst);
					}

					final SymbolStruct<?> parameterName = (SymbolStruct) listParameterFirst;
					final LispStruct parameterValue = listParameter.getRest().getFirst();

					// TODO: Why are we evaluating this in the outer environment??? I think i know, but not sure if it's actually needed
					final Environment currentEnvironment = SemanticAnalyzer.environmentStack.pop();
					final LispStruct parameterValueInitForm = SemanticAnalyzer.saMainLoop(parameterValue);
					SemanticAnalyzer.environmentStack.push(currentEnvironment);

					SemanticAnalyzer.bindingsPosition = EnvironmentAccessor.getNextAvailableParameterNumber(currentEnvironment);

					EnvironmentAccessor.createNewLetBinding(SemanticAnalyzer.environmentStack.peek(), parameterName, SemanticAnalyzer.bindingsPosition, parameterValueInitForm, false);
				} else if (currentParameter instanceof SymbolStruct) {
					final SymbolStruct<?> symbolParameter = (SymbolStruct) currentParameter;

					SemanticAnalyzer.bindingsPosition = EnvironmentAccessor.getNextAvailableParameterNumber(SemanticAnalyzer.environmentStack.peek());
					EnvironmentAccessor.createNewLetBinding(SemanticAnalyzer.environmentStack.peek(), symbolParameter, SemanticAnalyzer.bindingsPosition, NILStruct.INSTANCE, false);
				} else {
					throw new RuntimeException("LET: Parameter must be of type SymbolStruct or ListStruct. Got: " + currentParameter);
				}
			}

			final ListStruct body = input.getRest().getRest();

			final ListStruct prognList = new ConsStruct(SpecialOperator.PROGN, body);
			final ListStruct bodyResult = PrognAnalyzer.INSTANCE.analyze(prognList);

			final Environment envList = SemanticAnalyzer.environmentStack.peek();

			return new LetEnvironmentListStruct(envList, bodyResult);
		} finally {
			SemanticAnalyzer.bindingsPosition = tempPosition;
			SemanticAnalyzer.environmentStack.pop();
		}
	}
}
