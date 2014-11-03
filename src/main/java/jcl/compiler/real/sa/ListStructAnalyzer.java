package jcl.compiler.real.sa;

import jcl.LispStruct;
import jcl.compiler.real.environment.EnvironmentAccessor;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.lambdalist.KeyBinding;
import jcl.compiler.real.environment.lambdalist.OptionalBinding;
import jcl.compiler.real.environment.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.environment.lambdalist.RequiredBinding;
import jcl.compiler.real.environment.lambdalist.RestBinding;
import jcl.compiler.real.sa.specialoperator.special.LambdaAnalyzer;
import jcl.structs.conditions.exceptions.ProgramErrorException;
import jcl.structs.functions.FunctionStruct;
import jcl.structs.lists.ListStruct;
import jcl.structs.lists.NullStruct;
import jcl.structs.symbols.KeywordSymbolStruct;
import jcl.structs.symbols.SpecialOperator;
import jcl.structs.symbols.SymbolStruct;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.Stack;

public class ListStructAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final ListStructAnalyzer INSTANCE = new ListStructAnalyzer();

	@Override
	public LispStruct analyze(final ListStruct input, final SemanticAnalyzer analyzer) {

		if (input.equals(NullStruct.INSTANCE)) {
			return input;
		}

		final LispStruct first = input.getFirst();
		if (first instanceof SymbolStruct) {
			final Stack<Environment> environmentStack = analyzer.getEnvironmentStack();
//			final MacroExpandReturn macroExpandReturn = MacroExpandFunction.FUNCTION.funcall(input, environmentStack.peek());
			final LispStruct expandedForm = input; //macroExpandReturn.getExpandedForm(); // TODO: need to put something in place so this will work

			if (expandedForm.equals(NullStruct.INSTANCE)) {
				return NullStruct.INSTANCE;
			}

			if (expandedForm instanceof ListStruct) {
				final ListStruct expandedFormList = (ListStruct) expandedForm;

				final LispStruct expandedFormListFirst = expandedFormList.getFirst();
				if (expandedFormListFirst instanceof SpecialOperator) {
					return SpecialOperatorAnalyzer.INSTANCE.analyze(expandedFormList, analyzer);
				} else if (expandedFormListFirst instanceof SymbolStruct) {
					final SymbolStruct<?> functionSymbol = (SymbolStruct<?>) expandedFormListFirst;
					final ListStruct functionArguments = expandedFormList.getRest();
					return analyzeFunctionCall(analyzer, functionSymbol, functionArguments);
				} else {
					throw new ProgramErrorException("SA LIST: First element of expanded form must be of type SymbolStruct or ListStruct. Got: " + expandedFormListFirst);
				}
			} else {
				return analyzer.analyzeForm(expandedForm);
			}
		} else if (first instanceof ListStruct) {
			// ex ((lambda (x) (+ x 1)) 3)
			final ListStruct firstAsList = (ListStruct) first;

			final LispStruct firstOfFirstList = firstAsList.getFirst();
			if (firstOfFirstList.equals(SpecialOperator.LAMBDA)) {
				final LambdaEnvironmentListStruct lambdaAnalyzed = (LambdaEnvironmentListStruct) LambdaAnalyzer.INSTANCE.analyze(firstAsList, analyzer);
				final ListStruct functionArguments = input.getRest();
				return analyzedLambdaFunctionCall(analyzer, lambdaAnalyzed, functionArguments);
			} else {
				throw new ProgramErrorException("SA LIST: First element of a first element ListStruct must be the SpecialOperator 'LAMBDA'. Got: " + firstOfFirstList);
			}
		} else {
			throw new ProgramErrorException("SA LIST: First element must be of type SymbolStruct or ListStruct. Got: " + first);
		}
	}

	private static ListStruct analyzedLambdaFunctionCall(final SemanticAnalyzer semanticAnalyzer, final LambdaEnvironmentListStruct lambdaAnalyzed,
	                                                     final ListStruct functionArguments) {

		final List<LispStruct> analyzedFunctionList = new ArrayList<>();
		analyzedFunctionList.add(lambdaAnalyzed);

		final List<LispStruct> functionArgumentsList = functionArguments.getAsJavaList();

		final OrdinaryLambdaListBindings lambdaListBindings = lambdaAnalyzed.getLambdaListBindings();
		validateFunctionArguments("Anonymous Lambda", lambdaListBindings, functionArgumentsList);

		for (final LispStruct currentArgument : functionArgumentsList) {
			final LispStruct analyzedArgument = semanticAnalyzer.analyzeForm(currentArgument);
			analyzedFunctionList.add(analyzedArgument);
		}

		return ListStruct.buildProperList(analyzedFunctionList);
	}

	private static ListStruct analyzeFunctionCall(final SemanticAnalyzer semanticAnalyzer, final SymbolStruct<?> functionSymbol,
	                                              final ListStruct functionArguments) {

		final List<LispStruct> analyzedFunctionList = new ArrayList<>();

		final Stack<Environment> environmentStack = semanticAnalyzer.getEnvironmentStack();
		final boolean hasFunctionBinding = EnvironmentAccessor.hasFunctionBinding(environmentStack.peek(), functionSymbol);
		if (hasFunctionBinding) {
			// Recursive call
			analyzedFunctionList.add(SpecialOperator.TAIL_RECURSION);
		}
		analyzedFunctionList.add(functionSymbol);

		final List<LispStruct> functionArgumentsList = functionArguments.getAsJavaList();

		final Set<SymbolStruct<?>> undefinedFunctions = semanticAnalyzer.getUndefinedFunctions();

		final FunctionStruct function = functionSymbol.getFunction();
		if (function == null) {
			final Stack<SymbolStruct<?>> functionNameStack = semanticAnalyzer.getFunctionNameStack();

			if (functionNameStack.contains(functionSymbol)) {
				// Function is undefined, but name exists on the stack to be created
				undefinedFunctions.remove(functionSymbol);
			} else {
				// Add this as a possible undefined function
				undefinedFunctions.add(functionSymbol);
			}
		} else {
			// Function is defined.
			undefinedFunctions.remove(functionSymbol);

			final String functionName = functionSymbol.getName();
			final OrdinaryLambdaListBindings lambdaListBindings = function.getLambdaListBindings();
			validateFunctionArguments(functionName, lambdaListBindings, functionArgumentsList);
		}

		for (final LispStruct currentArgument : functionArgumentsList) {
			final LispStruct analyzedArgument = semanticAnalyzer.analyzeForm(currentArgument);
			analyzedFunctionList.add(analyzedArgument);
		}

		return ListStruct.buildProperList(analyzedFunctionList);
	}

	private static void validateFunctionArguments(final String functionName, final OrdinaryLambdaListBindings lambdaListBindings, final List<LispStruct> functionArguments) {
		final Iterator<LispStruct> functionArgumentsIterator = functionArguments.iterator();

		LispStruct nextArgument = null;

		final List<RequiredBinding> requiredBindings = lambdaListBindings.getRequiredBindings();
		for (final RequiredBinding ignored : requiredBindings) {
			if (!functionArgumentsIterator.hasNext()) {
				throw new ProgramErrorException("SA LIST: Too few arguments in call to '" + functionName + "'. " + functionArguments.size() + " arguments provided, at least " + requiredBindings.size() + " required.");
			}
			nextArgument = functionArgumentsIterator.next();
		}

		final List<OptionalBinding> optionalBindings = lambdaListBindings.getOptionalBindings();
		for (final OptionalBinding ignored : optionalBindings) {
			if (!functionArgumentsIterator.hasNext()) {
				break;
			}
			nextArgument = functionArgumentsIterator.next();
		}

		final List<KeyBinding> keyBindings = lambdaListBindings.getKeyBindings();
		final List<KeywordSymbolStruct> keys = new ArrayList<>(keyBindings.size());
		for (final KeyBinding keyBinding : keyBindings) {
			final KeywordSymbolStruct key = keyBinding.getKeyName();
			keys.add(key);
		}

		final RestBinding restBinding = lambdaListBindings.getRestBinding();

		while (functionArgumentsIterator.hasNext()) {
			if (!keyBindings.isEmpty()) {
				if (nextArgument instanceof KeywordSymbolStruct) {
					final KeywordSymbolStruct argumentKey = (KeywordSymbolStruct) nextArgument;
					if (keys.contains(argumentKey)) {
						// Consume the next argument
						functionArgumentsIterator.next();
						nextArgument = functionArgumentsIterator.next();
					} else {
						throw new ProgramErrorException("SA LIST: Keyword argument not found in '" + functionName + "' function definition: " + argumentKey);
					}
				} else {
					throw new ProgramErrorException("SA LIST: Expected Keyword argument for call to '" + functionName + " was: " + nextArgument);
				}
			} else if (restBinding != null) {
				functionArgumentsIterator.next();
			} else {
				throw new ProgramErrorException("SA LIST: Too many arguments in call to '" + functionName + "'. " + functionArguments.size() + " arguments provided, at most " + requiredBindings.size() + " accepted.");
			}
		}
	}
}
