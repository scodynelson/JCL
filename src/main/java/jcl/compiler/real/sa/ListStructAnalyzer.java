package jcl.compiler.real.sa;

import jcl.LispStruct;
import jcl.compiler.old.EnvironmentAccessor;
import jcl.compiler.old.functions.MacroExpandFunction;
import jcl.compiler.old.functions.MacroExpandReturn;
import jcl.compiler.real.environment.lambdalist.KeyBinding;
import jcl.compiler.real.environment.lambdalist.OptionalBinding;
import jcl.compiler.real.environment.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.environment.lambdalist.RequiredBinding;
import jcl.compiler.real.sa.specialoperator.special.LambdaAnalyzer;
import jcl.structs.functions.FunctionStruct;
import jcl.structs.lists.ConsStruct;
import jcl.structs.lists.ListStruct;
import jcl.structs.lists.NullStruct;
import jcl.structs.symbols.KeywordSymbolStruct;
import jcl.structs.symbols.SpecialOperator;
import jcl.structs.symbols.SymbolStruct;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class ListStructAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final ListStructAnalyzer INSTANCE = new ListStructAnalyzer();

	@Override
	public LispStruct analyze(final ListStruct input) {

		if (input.equals(NullStruct.INSTANCE)) {
			return input;
		}

		final LispStruct first = input.getFirst();
		if (first instanceof SymbolStruct) {
			final MacroExpandReturn macroExpandReturn = MacroExpandFunction.FUNCTION.funcall(input, SemanticAnalyzer.environmentStack.peek());
			final LispStruct expandedForm = macroExpandReturn.getExpandedForm();

			if (expandedForm.equals(NullStruct.INSTANCE)) {
				return NullStruct.INSTANCE;
			}

			if (expandedForm instanceof ListStruct) {
				final ListStruct expandedFormList = (ListStruct) expandedForm;

				final LispStruct expandedFormListFirst = expandedFormList.getFirst();
				if (expandedFormListFirst instanceof SpecialOperator) {
					return SpecialOperatorAnalyzer.INSTANCE.analyze(expandedFormList);
				} else if (expandedFormListFirst instanceof SymbolStruct) {
					final SymbolStruct<?> functionSymbol = (SymbolStruct<?>) expandedFormListFirst;
					final ListStruct functionArguments = expandedFormList.getRest();
					return analyzeFunctionMarker(functionSymbol, functionArguments);
				} else {
					throw new RuntimeException("SA LIST: First element of expanded form must be of type SymbolStruct or ListStruct. Got: " + expandedFormListFirst);
				}
			} else {
				return SemanticAnalyzer.saMainLoop(expandedForm);
			}
		} else if (first instanceof ListStruct) {
			// ex ((lambda (x) (+ x 1)) 3)
			final ListStruct firstAsList = (ListStruct) first;

			final LispStruct firstOfFirstList = firstAsList.getFirst();
			if (firstOfFirstList.equals(SpecialOperator.LAMBDA)) {
				final ListStruct lambdaAnalyzed = LambdaAnalyzer.INSTANCE.analyze(firstAsList);
				final ListStruct lambdaList = new ConsStruct(lambdaAnalyzed, input.getRest());

				return null; //analyzeFunctionMarker(lambdaList); TODO: lambda list to product function... hmm....
			} else {
				throw new RuntimeException("SA LIST: First element of a first element ListStruct must be the SpecialOperator 'LAMBDA'. Got: " + firstOfFirstList);
			}
		} else {
			throw new RuntimeException("SA LIST: First element must be of type SymbolStruct or ListStruct. Got: " + first);
		}
	}

	private ListStruct analyzeFunctionMarker(final SymbolStruct<?> functionSymbol, final ListStruct functionArguments) {

		final List<LispStruct> analyzedFunctionList = new ArrayList<>();

		final boolean hasFunctionBinding = EnvironmentAccessor.hasFunctionBinding(SemanticAnalyzer.environmentStack.peek(), functionSymbol);
		if (hasFunctionBinding) {
			// Recursive call
			analyzedFunctionList.add(SpecialOperator.TAIL_RECURSION);
		}
		analyzedFunctionList.add(functionSymbol);

		final List<LispStruct> functionArgumentsList = functionArguments.getAsJavaList();

		final FunctionStruct function = functionSymbol.getFunction();
		if (function == null) {
			// add this as a possible undefined function
			SemanticAnalyzer.undefinedFunctions.add(functionSymbol);
		} else {
			SemanticAnalyzer.undefinedFunctions.remove(functionSymbol);
			validateFunctionArguments(functionSymbol, function, functionArgumentsList);
		}

		for (final LispStruct currentArgument : functionArgumentsList) {
			final LispStruct analyzedArgument = SemanticAnalyzer.saMainLoop(currentArgument);
			analyzedFunctionList.add(analyzedArgument);
		}

		return ListStruct.buildProperList(analyzedFunctionList);
	}

	private static void validateFunctionArguments(final SymbolStruct<?> functionSymbol, final FunctionStruct function, final List<LispStruct> functionArguments) {
		// TODO: analyze by type. But how do we do this when the result to be checked for type can be dynamically
		// TODO: determined by a function call as a function argument... hmm....

		final OrdinaryLambdaListBindings ordinaryLambdaListBindings = function.getOrdinaryLambdaListBindings();

		final Iterator<LispStruct> functionArgumentsIterator = functionArguments.iterator();

		LispStruct nextArgument = null;

		final List<RequiredBinding> requiredBindings = ordinaryLambdaListBindings.getRequiredBindings();
		for (final RequiredBinding ignored : requiredBindings) {
			if (!functionArgumentsIterator.hasNext()) {
				throw new RuntimeException("SA LIST: Too few arguments in call to '" + functionSymbol.getName() + "'. " + functionArguments.size() + " arguments provided, at least " + requiredBindings.size() + " required.");
			}
			nextArgument = functionArgumentsIterator.next();
		}

		final List<OptionalBinding> optionalBindings = ordinaryLambdaListBindings.getOptionalBindings();
		for (final OptionalBinding ignored : optionalBindings) {
			if (!functionArgumentsIterator.hasNext()) {
				break;
			}
			nextArgument = functionArgumentsIterator.next();
		}

		final List<KeyBinding> keyBindings = ordinaryLambdaListBindings.getKeyBindings();
		final List<KeywordSymbolStruct> keys = new ArrayList<>(keyBindings.size());
		for (final KeyBinding keyBinding : keyBindings) {
			final KeywordSymbolStruct key = keyBinding.getKeyName();
			keys.add(key);
		}

		while (functionArgumentsIterator.hasNext()) {
			if (!keys.isEmpty()) {
				if (nextArgument instanceof KeywordSymbolStruct) {
					final KeywordSymbolStruct argumentKey = (KeywordSymbolStruct) nextArgument;
					if (keys.contains(argumentKey)) {
						// Consume the next argument
						functionArgumentsIterator.next();
					} else {
						throw new RuntimeException("SA LIST: Keyword argument not found in '" + functionSymbol.getName() + "' function definition: " + argumentKey);
					}
				} else {
					throw new RuntimeException("SA LIST: Expected Keyword argument for call to '" + functionSymbol.getName() + " was: " + nextArgument);
				}
			}

			nextArgument = functionArgumentsIterator.next();
		}
	}
}
