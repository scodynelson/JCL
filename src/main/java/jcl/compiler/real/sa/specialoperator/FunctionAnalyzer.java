package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.environment.EnvironmentAccessor;
import jcl.compiler.real.environment.Binding;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.Marker;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.SymbolStructAnalyzer;
import jcl.compiler.real.sa.specialoperator.special.LambdaAnalyzer;
import jcl.structs.conditions.exceptions.ProgramErrorException;
import jcl.structs.lists.ConsStruct;
import jcl.structs.lists.ListStruct;
import jcl.structs.symbols.SpecialOperator;
import jcl.structs.symbols.SymbolStruct;

import java.util.Stack;

public class FunctionAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final FunctionAnalyzer INSTANCE = new FunctionAnalyzer();

	@Override
	public ListStruct analyze(final ListStruct input, final SemanticAnalyzer analyzer) {

		if (input.size() != 2) {
			throw new ProgramErrorException("FUNCTION: Incorrect number of arguments: " + input.size() + ". Expected 2 arguments.");
		}

		final LispStruct second = input.getRest().getFirst();
		if (!(second instanceof SymbolStruct) && !(second instanceof ListStruct)) {
			throw new ProgramErrorException("FUNCTION: Function argument must be of type SymbolStruct or ListStruct. Got: " + second);
		}

		final Stack<Environment> environmentStack = analyzer.getEnvironmentStack();
		final Environment parentEnvironment = environmentStack.peek();

		if (second instanceof SymbolStruct) {
			final SymbolStruct<?> functionSymbol = (SymbolStruct) second;
			final Environment fnBinding = EnvironmentAccessor.getBindingEnvironment(parentEnvironment, functionSymbol, false);

			if (fnBinding.equals(Environment.NULL)) {
				SymbolStructAnalyzer.INSTANCE.analyze(functionSymbol, analyzer);
				return input;
			} else {
				final Binding binding = fnBinding.getBinding(functionSymbol);
				final SymbolStruct<?> functionBindingName = binding.getSymbolStruct();

				final LispStruct first = input.getFirst();
				return new ConsStruct(first, functionBindingName);
			}
		} else {
			final ListStruct functionList = (ListStruct) second;
			final LispStruct functionListFirst = functionList.getFirst();

			if (functionListFirst.equals(SpecialOperator.LAMBDA)) {
				final int tempClosureDepth = analyzer.getClosureDepth();
				final int newClosureDepth = tempClosureDepth + 1;

				final Environment lambdaEnvironment = EnvironmentAccessor.createNewEnvironment(parentEnvironment, Marker.LAMBDA, newClosureDepth);
				environmentStack.push(lambdaEnvironment);

				final int tempBindingsPosition = analyzer.getBindingsPosition();
				try {
					return LambdaAnalyzer.INSTANCE.analyze(functionList, analyzer);
				} finally {
					analyzer.setClosureDepth(tempClosureDepth);
					analyzer.setBindingsPosition(tempBindingsPosition);
					environmentStack.pop();
				}
			}

			throw new ProgramErrorException("FUNCTION: First element of List argument must be the Symbol LAMBDA. Got: " + functionListFirst);
		}
	}
}
