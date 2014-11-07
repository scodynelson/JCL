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

public class MacroletAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final MacroletAnalyzer INSTANCE = new MacroletAnalyzer();

	@Override
	public LispStruct analyze(final ListStruct input, final SemanticAnalyzer analyzer) {

		if (input.size() < 2) {
			throw new ProgramErrorException("MACROLET: Incorrect number of arguments: " + input.size() + ". Expected at least 2 arguments.");
		}

		final LispStruct second = input.getRest().getFirst();
		if (!(second instanceof ListStruct)) {
			throw new ProgramErrorException("MACROLET: Parameter list must be of type ListStruct. Got: " + second);
		}

		final Stack<Environment> environmentStack = analyzer.getEnvironmentStack();
		final Environment parentEnvironment = environmentStack.peek();

		final int tempClosureDepth = analyzer.getClosureDepth();
		final int newClosureDepth = tempClosureDepth + 1;

		final Environment macroletEnvironment = EnvironmentAccessor.createNewEnvironment(parentEnvironment, Marker.MACROLET, newClosureDepth);
		environmentStack.push(macroletEnvironment);

		final Stack<SymbolStruct<?>> functionNameStack = analyzer.getFunctionNameStack();
		List<SymbolStruct<?>> macroNames = null;

		final int tempBindingsPosition = analyzer.getBindingsPosition();
		try {
			final ListStruct macroletMacros = (ListStruct) second;
			final List<LispStruct> macroletMacrosJavaList = macroletMacros.getAsJavaList();
			macroNames = getMacroNames(macroletMacrosJavaList);

			// Add macro names BEFORE analyzing the functions
			StackUtils.pushAll(functionNameStack, macroNames);

			for (final LispStruct currentMacro : macroletMacrosJavaList) {
				if (!(currentMacro instanceof ListStruct)) {
					throw new ProgramErrorException("MACROLET: Macro parameter must be of type ListStruct. Got: " + currentMacro);
				}
				final ListStruct macroList = (ListStruct) currentMacro;

				final LispStruct macroListFirst = macroList.getFirst();
				if (!(macroListFirst instanceof SymbolStruct)) {
					throw new ProgramErrorException("MACROLET: Macro parameter first element value must be of type SymbolStruct. Got: " + macroListFirst);
				}
				final SymbolStruct<?> macroName = (SymbolStruct) macroListFirst;

				final LispStruct macroListSecond = macroList.getRest().getFirst();
				if (!(macroListSecond instanceof ListStruct)) {
					throw new ProgramErrorException("MACROLET: Macro parameter second element value must be of type ListStruct. Got: " + macroListSecond);
				}

				final ListStruct lambdaList = (ListStruct) macroListSecond;
				final ListStruct body = macroList.getRest().getRest();

				final List<LispStruct> innerBlock = new ArrayList<>();
				innerBlock.add(SpecialOperator.BLOCK);
				innerBlock.add(macroName);
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

				// Evaluate in the current environment. This is one of the differences between Flet and Macrolet.
				final Environment currentEnvironment = environmentStack.peek();
				final LispStruct paramValueInitForm = analyzer.analyzeForm(innerFunctionListStruct);

				final int newBindingsPosition = EnvironmentAccessor.getNextAvailableParameterNumber(currentEnvironment);
				analyzer.setBindingsPosition(newBindingsPosition);

				EnvironmentAccessor.createNewEnvironmentBinding(currentEnvironment, macroName, newBindingsPosition, paramValueInitForm, false);
			}

			final ListStruct currentBodyForms = input.getRest().getRest();
			final BodyProcessingResult bodyProcessingResult = BodyWithDeclaresAnalyzer.INSTANCE.analyze(currentBodyForms, analyzer);

			final Environment envList = environmentStack.peek();

			final ListStruct newBodyForms = ListStruct.buildProperList(bodyProcessingResult.getBodyForms());
			return new EnvironmentListStruct(envList, bodyProcessingResult.getDeclarations(), newBodyForms);
		} finally {
			if (macroNames != null) {
				StackUtils.popX(functionNameStack, macroNames.size());
			}

			analyzer.setClosureDepth(tempClosureDepth);
			analyzer.setBindingsPosition(tempBindingsPosition);
			environmentStack.pop();
		}
	}

	private List<SymbolStruct<?>> getMacroNames(final List<LispStruct> macroDefList) {

		final List<SymbolStruct<?>> macroNames = new ArrayList<>(macroDefList.size());

		for (final LispStruct currentMacroDef : macroDefList) {
			if (!(currentMacroDef instanceof ListStruct)) {
				throw new ProgramErrorException("MACROLET: Macro parameter must be of type ListStruct. Got: " + currentMacroDef);
			}
			final ListStruct macroList = (ListStruct) currentMacroDef;

			final LispStruct macroListFirst = macroList.getFirst();
			if (!(macroListFirst instanceof SymbolStruct)) {
				throw new ProgramErrorException("MACROLET: Macro parameter first element value must be of type SymbolStruct. Got: " + macroListFirst);
			}

			final SymbolStruct<?> macroName = (SymbolStruct) macroListFirst;
			macroNames.add(macroName);
		}

		return macroNames;
	}
}
