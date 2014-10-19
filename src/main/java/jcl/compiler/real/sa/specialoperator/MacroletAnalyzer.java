package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.old.EnvironmentAccessor;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.Marker;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.MacroletEnvironmentListStruct;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.structs.lists.ConsStruct;
import jcl.structs.lists.ListStruct;
import jcl.structs.symbols.SpecialOperator;
import jcl.structs.symbols.SymbolStruct;

import java.util.ArrayList;
import java.util.List;

public class MacroletAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final MacroletAnalyzer INSTANCE = new MacroletAnalyzer();

	@Override
	public LispStruct analyze(final ListStruct input) {

		if (input.size() < 2) {
			throw new RuntimeException("MACROLET: Incorrect number of arguments: " + input.size() + ". Expected at least 2 arguments.");
		}

		final LispStruct second = input.getRest().getFirst();
		if (!(second instanceof ListStruct)) {
			throw new RuntimeException("MACROLET: Parameter list must be of type ListStruct. Got: " + second);
		}

		final Environment parentEnvironment = SemanticAnalyzer.environmentStack.peek();

		final Environment macroletEnvironment = EnvironmentAccessor.createNewEnvironment(Marker.MACROLET);
		macroletEnvironment.setParent(parentEnvironment);

		SemanticAnalyzer.environmentStack.push(macroletEnvironment);

		// TODO: not sure if this account for prior macros being able to resolve later macros...

		final int tempPosition = SemanticAnalyzer.bindingsPosition;
		try {
			final ListStruct macroletMacros = input.getRest();
			final List<LispStruct> macroletMacrosJavaList = macroletMacros.getAsJavaList();

			for (final LispStruct currentMacro : macroletMacrosJavaList) {
				if (!(currentMacro instanceof ListStruct)) {
					throw new RuntimeException("MACROLET: Macro parameter must be of type ListStruct. Got: " + currentMacro);
				}
				final ListStruct macroList = (ListStruct) currentMacro;

				final LispStruct macroListFirst = macroList.getFirst();
				if (!(macroListFirst instanceof SymbolStruct)) {
					throw new RuntimeException("MACROLET: Macro parameter first element value must be of type SymbolStruct. Got: " + macroListFirst);
				}
				final SymbolStruct<?> macroName = (SymbolStruct) macroListFirst;

				final LispStruct macroListSecond = macroList.getRest().getFirst();
				if (!(macroListSecond instanceof ListStruct)) {
					throw new RuntimeException("MACROLET: Macro parameter second element value must be of type ListStruct. Got: " + macroListSecond);
				}

				final ListStruct lambdaList = (ListStruct) macroListSecond;
				final ListStruct body = macroList.getRest().getRest();

				final List<LispStruct> innerBlock = new ArrayList<>();
				innerBlock.add(SpecialOperator.BLOCK);
				innerBlock.add(macroName);
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

				// Evaluate in the current environment. This is the difference between Flet and Labels.
				final Environment currentEnvironment = SemanticAnalyzer.environmentStack.peek();
				final LispStruct paramValueInitForm = SemanticAnalyzer.saMainLoop(innerFunctionListStruct);

				SemanticAnalyzer.bindingsPosition = EnvironmentAccessor.getNextAvailableParameterNumber(currentEnvironment);

				EnvironmentAccessor.createNewLetBinding(currentEnvironment, macroName, SemanticAnalyzer.bindingsPosition, paramValueInitForm, false);
			}

			final ListStruct body = input.getRest().getRest();

			final ListStruct prognList = new ConsStruct(SpecialOperator.PROGN, body);
			final ListStruct bodyResult = PrognAnalyzer.INSTANCE.analyze(prognList);

			final Environment envList = SemanticAnalyzer.environmentStack.peek();

			return new MacroletEnvironmentListStruct(envList, bodyResult);
		} finally {
			SemanticAnalyzer.bindingsPosition = tempPosition;
			SemanticAnalyzer.environmentStack.pop();
		}
	}
}
