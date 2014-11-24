package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.environment.EnvironmentAccessor;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.Marker;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.EnvironmentLispStruct;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.specialoperator.body.BodyProcessingResult;
import jcl.compiler.real.sa.specialoperator.body.BodyWithDeclaresAnalyzer;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;

import java.util.List;
import java.util.Stack;

public class LetAnalyzer implements Analyzer<EnvironmentLispStruct, ListStruct> {

	public static final LetAnalyzer INSTANCE = new LetAnalyzer();

	@Override
	public EnvironmentLispStruct analyze(final ListStruct input, final SemanticAnalyzer analyzer) {

		if (input.size() < 2) {
			throw new ProgramErrorException("LET: Incorrect number of arguments: " + input.size() + ". Expected at least 2 arguments.");
		}

		final LispStruct second = input.getRest().getFirst();
		if (!(second instanceof ListStruct)) {
			throw new ProgramErrorException("LET: Parameter list must be of type ListStruct. Got: " + second);
		}

		final Stack<Environment> environmentStack = analyzer.getEnvironmentStack();
		final Environment parentEnvironment = environmentStack.peek();

		final int tempClosureDepth = analyzer.getClosureDepth();
		final int newClosureDepth = tempClosureDepth + 1;

		final Environment letEnvironment = EnvironmentAccessor.createNewEnvironment(parentEnvironment, Marker.LET, newClosureDepth);
		environmentStack.push(letEnvironment);

		final int tempBindingsPosition = analyzer.getBindingsPosition();
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
						parameterValueInitForm = analyzer.analyzeForm(parameterValue);
					} finally {
						environmentStack.push(currentEnvironment);
					}

					final int newBindingsPosition = EnvironmentAccessor.getNextAvailableParameterNumber(currentEnvironment);
					analyzer.setBindingsPosition(newBindingsPosition);

					EnvironmentAccessor.createNewEnvironmentBinding(environmentStack.peek(), parameterName, newBindingsPosition, parameterValueInitForm, false);
				} else if (currentParameter instanceof SymbolStruct) {
					final SymbolStruct<?> symbolParameter = (SymbolStruct) currentParameter;

					final int newBindingsPosition = EnvironmentAccessor.getNextAvailableParameterNumber(environmentStack.peek());
					analyzer.setBindingsPosition(newBindingsPosition);

					EnvironmentAccessor.createNewEnvironmentBinding(environmentStack.peek(), symbolParameter, newBindingsPosition, NILStruct.INSTANCE, false);
				} else {
					throw new ProgramErrorException("LET: Parameter must be of type SymbolStruct or ListStruct. Got: " + currentParameter);
				}
			}

			final ListStruct currentBodyForms = input.getRest().getRest();
			final BodyProcessingResult bodyProcessingResult = BodyWithDeclaresAnalyzer.INSTANCE.analyze(currentBodyForms, analyzer);

			final Environment envList = environmentStack.peek();

			final ListStruct newBodyForms = ListStruct.buildProperList(bodyProcessingResult.getBodyForms());
			return new EnvironmentLispStruct(envList, bodyProcessingResult.getDeclarations(), newBodyForms);
		} finally {
			analyzer.setClosureDepth(tempClosureDepth);
			analyzer.setBindingsPosition(tempBindingsPosition);
			environmentStack.pop();
		}
	}
}
