package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.old.EnvironmentAccessor;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.Marker;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;

import java.util.List;

public class LetAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final LetAnalyzer INSTANCE = new LetAnalyzer();

	@Override
	public LispStruct analyze(final ListStruct input) {

		if (input.size() == 1) {
			throw new RuntimeException("Wrong number of arguments to special operator LET: " + input.size());
		}

		final LispStruct second = input.getRest().getFirst();
		if (second instanceof ListStruct) {

			// Create the list to hold the new lambda expression.
			// --------------------------
			// Semantic Analysis

			// keep track of the current environment as it is "now"
			final Environment prevEnvironment = SemanticAnalyzer.environmentStack.peek();
			// make a new environment and set it to "current"
			final Environment newLetEnvironment = EnvironmentAccessor.createNewEnvironment(Marker.LET);
			newLetEnvironment.setParent(prevEnvironment);
			// set the current environment's parent to what was the environment
			SemanticAnalyzer.environmentStack.push(newLetEnvironment);

			final int tempPosition = SemanticAnalyzer.bindingsPosition;
			try {
				// Create the vectors for the SymbolStructs and their values.

				// LET must have at least one parameter.
				final ListStruct parameters = (ListStruct) second;
				final List<LispStruct> parametersJavaList = parameters.getAsJavaList();

				// Now build a list containing all the local VariableOlds for the LET,
				// and a list containing the initial values for the local VariableOlds.

				// (...bindings...)

				// Loop through the SymbolStructs and store them.
				//first we have to find out the first available open local slot number
				for (final LispStruct currentParameter : parametersJavaList) {
					if (currentParameter instanceof ListStruct) {
						final ListStruct listParameter = (ListStruct) currentParameter;
						if ((listParameter.size() < 1) || (listParameter.size() > 2)) {
							throw new RuntimeException("Improperly Formed Let: the list parameters must only have 1 or 2 parameters");
						}

						final LispStruct listParamFirst = listParameter.getFirst();
						if (!(listParamFirst instanceof SymbolStruct)) {
							throw new RuntimeException("Improperly Formed Let: the first list parameters must be a SymbolStruct");
						}

						final SymbolStruct<?> symbolParam = (SymbolStruct) listParamFirst;
						final LispStruct symbolParamValue = listParameter.getRest().getFirst();

						// this must be done in the context of the outer environment
						final Environment tmpCurrent = SemanticAnalyzer.environmentStack.pop();
						final LispStruct symbolParamInitForm = SemanticAnalyzer.saMainLoop(symbolParamValue);
						SemanticAnalyzer.environmentStack.push(tmpCurrent);

						SemanticAnalyzer.bindingsPosition = EnvironmentAccessor.getNextAvailableParameterNumber(tmpCurrent);

						EnvironmentAccessor.createNewLetBinding(SemanticAnalyzer.environmentStack.peek(), symbolParam, SemanticAnalyzer.bindingsPosition, symbolParamInitForm, false);
					} else if (currentParameter instanceof SymbolStruct) {
						final SymbolStruct<?> symbolParameter = (SymbolStruct) currentParameter;

						SemanticAnalyzer.bindingsPosition = EnvironmentAccessor.getNextAvailableParameterNumber(SemanticAnalyzer.environmentStack.peek());
						EnvironmentAccessor.createNewLetBinding(SemanticAnalyzer.environmentStack.peek(), symbolParameter, SemanticAnalyzer.bindingsPosition, NullStruct.INSTANCE, false);
					} else {
						throw new RuntimeException("Improperly Formed Let: the parameters must be either a ListStruct or SymbolStruct");
					}
				}

				final ListStruct body = input.getRest().getRest();

				final ListStruct prognList = new ConsStruct(SpecialOperator.PROGN, body);
				final LispStruct bodyResult = PrognAnalyzer.INSTANCE.analyze(prognList);

				// Now we have to reverse the list of bindings since they are pushed on
				final Environment envList = SemanticAnalyzer.environmentStack.peek();

				return new ConsStruct(envList, bodyResult);
			} finally {
				SemanticAnalyzer.bindingsPosition = tempPosition;
				SemanticAnalyzer.environmentStack.pop();
			}
		}

		throw new RuntimeException("Improperly Formed Let: the parameter list must be a ListStruct");
	}
}
