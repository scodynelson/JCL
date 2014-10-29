package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.old.EnvironmentAccessor;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.Marker;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.EnvironmentListStruct;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.structs.conditions.exceptions.ProgramErrorException;
import jcl.structs.lists.ConsStruct;
import jcl.structs.lists.ListStruct;
import jcl.structs.symbols.NILStruct;
import jcl.structs.symbols.SpecialOperator;
import jcl.structs.symbols.SymbolStruct;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class LetAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final LetAnalyzer INSTANCE = new LetAnalyzer();

	@Override
	public ListStruct analyze(final ListStruct input) {

		if (input.size() < 2) {
			throw new ProgramErrorException("LET: Incorrect number of arguments: " + input.size() + ". Expected at least 2 arguments.");
		}

		final LispStruct second = input.getRest().getFirst();
		if (!(second instanceof ListStruct)) {
			throw new ProgramErrorException("LET: Parameter list must be of type ListStruct. Got: " + second);
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
						throw new ProgramErrorException("LET: ListStruct parameter must have only 1 or 2 elements. Got: " + currentParameter);
					}

					final LispStruct listParameterFirst = listParameter.getFirst();
					if (!(listParameterFirst instanceof SymbolStruct)) {
						throw new ProgramErrorException("LET: ListStruct parameter first element value must be of type SymbolStruct. Got: " + listParameterFirst);
					}

					final SymbolStruct<?> parameterName = (SymbolStruct) listParameterFirst;
					final LispStruct parameterValue = listParameter.getRest().getFirst();

					// Evaluate in the outer environment. This is because we want to ensure we don't have references to symbols that may not exist.
					final Environment currentEnvironment = SemanticAnalyzer.environmentStack.pop();

					final LispStruct parameterValueInitForm;
					try {
						parameterValueInitForm = SemanticAnalyzer.saMainLoop(parameterValue);
					} finally {
						SemanticAnalyzer.environmentStack.push(currentEnvironment);
					}

					SemanticAnalyzer.bindingsPosition = EnvironmentAccessor.getNextAvailableParameterNumber(currentEnvironment);

					EnvironmentAccessor.createNewLetBinding(SemanticAnalyzer.environmentStack.peek(), parameterName, SemanticAnalyzer.bindingsPosition, parameterValueInitForm, false);
				} else if (currentParameter instanceof SymbolStruct) {
					final SymbolStruct<?> symbolParameter = (SymbolStruct) currentParameter;

					SemanticAnalyzer.bindingsPosition = EnvironmentAccessor.getNextAvailableParameterNumber(SemanticAnalyzer.environmentStack.peek());
					EnvironmentAccessor.createNewLetBinding(SemanticAnalyzer.environmentStack.peek(), symbolParameter, SemanticAnalyzer.bindingsPosition, NILStruct.INSTANCE, false);
				} else {
					throw new ProgramErrorException("LET: Parameter must be of type SymbolStruct or ListStruct. Got: " + currentParameter);
				}
			}

			final ListStruct currentBodyForms = input.getRest().getRest();

			final List<LispStruct> currentBodyFormsAsJavaList = currentBodyForms.getAsJavaList();
			final Iterator<LispStruct> currentBodyFormsIterator = currentBodyFormsAsJavaList.iterator();

			final List<LispStruct> declarations = new ArrayList<>();
			final List<LispStruct> newBodyForms = new ArrayList<>();

			while (currentBodyFormsIterator.hasNext()) {
				final LispStruct currentForm = currentBodyFormsIterator.next();

				if (!newBodyForms.isEmpty()) {
					newBodyForms.add(currentForm);
					continue;
				}

				if (currentForm instanceof ListStruct) {
					final ListStruct currentFormAsList = (ListStruct) currentForm;

					final LispStruct firstOfCurrentForm = currentFormAsList.getFirst();
					if (firstOfCurrentForm.equals(SpecialOperator.DECLARE)) {
						declarations.add(currentForm);
					} else {
						newBodyForms.add(currentForm);
					}
				} else {
					newBodyForms.add(currentForm);
				}
			}

			final ListStruct newBodyFormsLL = ListStruct.buildProperList(newBodyForms);
			final ListStruct bodyResult = PrognAnalyzer.INSTANCE.analyze(newBodyFormsLL);

			final Environment envList = SemanticAnalyzer.environmentStack.peek();

			return new EnvironmentListStruct(envList, declarations, bodyResult);
		} finally {
			SemanticAnalyzer.bindingsPosition = tempPosition;
			SemanticAnalyzer.environmentStack.pop();
		}
	}
}
