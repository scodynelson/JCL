package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.environment.Binding;
import jcl.compiler.real.environment.LexicalEnvironment;
import jcl.compiler.real.environment.Marker;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.LexicalSymbolStructAnalyzer;
import jcl.compiler.real.sa.specialoperator.special.LambdaAnalyzer;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Optional;
import java.util.Stack;

@Component
public class FunctionAnalyzer implements SpecialOperatorAnalyzer {

	@Autowired
	private LexicalSymbolStructAnalyzer lexicalSymbolStructAnalyzer;

	@Autowired
	private LambdaAnalyzer lambdaAnalyzer;

	@Override
	public LispStruct analyze(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder) {

		if (input.size() != 2) {
			throw new ProgramErrorException("FUNCTION: Incorrect number of arguments: " + input.size() + ". Expected 2 arguments.");
		}

		final LispStruct second = input.getRest().getFirst();
		if (!(second instanceof SymbolStruct) && !(second instanceof ListStruct)) {
			throw new ProgramErrorException("FUNCTION: Function argument must be of type SymbolStruct or ListStruct. Got: " + second);
		}

		final Stack<LexicalEnvironment> lexicalEnvironmentStack = analysisBuilder.getLexicalEnvironmentStack();
		final LexicalEnvironment parentLexicalEnvironment = lexicalEnvironmentStack.peek();

		if (second instanceof SymbolStruct) {
			final SymbolStruct<?> functionSymbol = (SymbolStruct) second;
			final LexicalEnvironment fnBinding = getBindingEnvironment(parentLexicalEnvironment, functionSymbol);

			if (fnBinding.equals(LexicalEnvironment.NULL)) {
				// TODO: we should think about what's actually happening here...
				lexicalSymbolStructAnalyzer.analyzeSymbol(functionSymbol, analysisBuilder);
				return input;
			} else {
				final Optional<Binding> binding = fnBinding.getBinding(functionSymbol);

				// TODO: Refactor this a bit. This should always be true at this point. BUT, we DO want to make sure we're
				// TODO:    not searching the NULL environment for the binding. Hmm...
				if (binding.isPresent()) {
					final Binding bindingValue = binding.get();
					final SymbolStruct<?> functionBindingName = bindingValue.getSymbolStruct();

					final LispStruct first = input.getFirst();
					return new ConsStruct(first, functionBindingName);
				}

				//TODO: what do we do here???
				throw new ProgramErrorException("FUNCTION: Failed to find function symbol binding in environment.");
			}
		}

		final ListStruct functionList = (ListStruct) second;
		final LispStruct functionListFirst = functionList.getFirst();

		if (functionListFirst.equals(SpecialOperator.LAMBDA)) {
			final int tempClosureDepth = analysisBuilder.getClosureDepth();
			final int newClosureDepth = tempClosureDepth + 1;

			final LexicalEnvironment lambdaEnvironment = new LexicalEnvironment(parentLexicalEnvironment, Marker.LAMBDA, newClosureDepth);
			lexicalEnvironmentStack.push(lambdaEnvironment);

			final int tempBindingsPosition = analysisBuilder.getBindingsPosition();
			try {
				analysisBuilder.setClosureDepth(newClosureDepth);

				return lambdaAnalyzer.analyze(analyzer, functionList, analysisBuilder);
			} finally {
				analysisBuilder.setClosureDepth(tempClosureDepth);
				analysisBuilder.setBindingsPosition(tempBindingsPosition);
				lexicalEnvironmentStack.pop();
			}
		}

		throw new ProgramErrorException("FUNCTION: First element of List argument must be the Symbol LAMBDA. Got: " + functionListFirst);
	}

	private static LexicalEnvironment getBindingEnvironment(final LexicalEnvironment lexicalEnvironment, final SymbolStruct<?> variable) {

		LexicalEnvironment currentLexicalEnvironment = lexicalEnvironment;

		while (!currentLexicalEnvironment.equals(LexicalEnvironment.NULL)) {

			final Marker marker = currentLexicalEnvironment.getMarker();
			if (Marker.FUNCTION_MARKERS.contains(marker)) {

				final boolean hasBinding = currentLexicalEnvironment.hasBinding(variable);
				if (hasBinding) {
					break;
				}
			}

			currentLexicalEnvironment = currentLexicalEnvironment.getParent();
		}

		return currentLexicalEnvironment;
	}

}
