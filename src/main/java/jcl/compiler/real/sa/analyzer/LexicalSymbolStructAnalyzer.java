package jcl.compiler.real.sa.analyzer;

import jcl.compiler.real.environment.Closure;
import jcl.compiler.real.environment.ClosureBinding;
import jcl.compiler.real.environment.EnvironmentAllocation;
import jcl.compiler.real.environment.LexicalEnvironment;
import jcl.compiler.real.environment.Marker;
import jcl.compiler.real.environment.Scope;
import jcl.compiler.real.environment.SymbolBinding;
import jcl.compiler.real.environment.SymbolTable;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.element.SymbolElement;
import jcl.symbols.SymbolStruct;
import jcl.types.T;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Optional;
import java.util.Stack;

@Component
public class LexicalSymbolStructAnalyzer extends SymbolStructAnalyzer {

	private static final long serialVersionUID = 231543795392423102L;

	@Autowired
	private DynamicSymbolStructAnalyzer dynamicSymbolStructAnalyzer;

	@Override
	public SymbolElement<?> analyze(final SemanticAnalyzer analyzer, final SymbolStruct<?> input, final AnalysisBuilder analysisBuilder) {

		final Stack<LexicalEnvironment> lexicalEnvironmentStack = analysisBuilder.getLexicalEnvironmentStack();
		final LexicalEnvironment currentLexicalEnvironment = lexicalEnvironmentStack.peek();

		final boolean symbolBoundInCurrentLexicalEnvironment = currentLexicalEnvironment.hasBinding(input);
		if (symbolBoundInCurrentLexicalEnvironment) {
			// Binding already exists in the current lexical environment.
			return new SymbolElement<>(input);
		}

		final LexicalEnvironment bindingLexicalEnvironment = getBindingEnvironment(currentLexicalEnvironment, input);

		if (bindingLexicalEnvironment.equals(LexicalEnvironment.NULL)) {
			// No inner binding lexical environments. Add it as a DYNAMIC symbol in the current lexical environment before we proceed.
			dynamicSymbolStructAnalyzer.analyze(analyzer, input, analysisBuilder);
		}

		final LexicalEnvironment currentEnclosingLambda = getEnclosingLambda(currentLexicalEnvironment);
		final LexicalEnvironment bindingEnclosingLambda = getEnclosingLambda(bindingLexicalEnvironment);

		final SymbolTable currentLexicalEnvironmentSymbolTable = currentLexicalEnvironment.getSymbolTable();

		if (currentEnclosingLambda.equals(bindingEnclosingLambda)) {
			// Binding Lambda and Enclosing Lambda are the same. No need for a Closure.

			// Create a new SymbolBinding and reference it to the 'bindingLexicalEnvironment'
			final EnvironmentAllocation allocation = new EnvironmentAllocation(bindingEnclosingLambda);
			final SymbolBinding symbolBinding = new SymbolBinding(input, allocation, Scope.LEXICAL, T.INSTANCE, bindingLexicalEnvironment);

			// Now add that new symbol to the SymbolTable of the 'currentLexicalEnvironment'
			currentLexicalEnvironmentSymbolTable.addBinding(symbolBinding);
			return new SymbolElement<>(input);
		}

		// Here the Binding Lambda is outside of the Enclosing Lambda
		final LexicalEnvironment outerBindingLexicalEnvironment = getBindingEnvironment(bindingLexicalEnvironment, input);

		if (outerBindingLexicalEnvironment.equals(LexicalEnvironment.NULL)) {
			// Outer Binding Lexical Environment is the NULL Environment. Therefore, we can't create a Closure.

			// Create a new SymbolBinding and reference it to the 'bindingLexicalEnvironment'
			final EnvironmentAllocation allocation = new EnvironmentAllocation(LexicalEnvironment.NULL);
			final SymbolBinding symbolBinding = new SymbolBinding(input, allocation, Scope.LEXICAL, T.INSTANCE, bindingLexicalEnvironment);

			// Now add that new symbol to the SymbolTable of the 'currentLexicalEnvironment'
			currentLexicalEnvironmentSymbolTable.addBinding(symbolBinding);
			return new SymbolElement<>(input);
		}

		// There is an Outer Binding Lexical Environment. Therefore, we will create a Closure Binding in that Lexical Environment.
		final Closure closure = outerBindingLexicalEnvironment.getEnvironmentClosure();

		final Optional<ClosureBinding> closureBinding = closure.getBinding(input);

		if (closureBinding.isPresent()) {
			// Closure Binding already exists in the Outer Binding Environment.

			final ClosureBinding closureBindingValue = closureBinding.get();

			// Increment the number of references to this Closure Binding.
			closureBindingValue.incrementReferences();
		} else {
			// Create a new ClosureBinding in the Outer Binding Lexical Environment.
			final ClosureBinding newClosureBinding = new ClosureBinding(input, closure.getBindings().size(), 1);
			closure.addBinding(newClosureBinding);
		}

		return new SymbolElement<>(input);
	}

	private static LexicalEnvironment getBindingEnvironment(final LexicalEnvironment lexicalEnvironment, final SymbolStruct<?> variable) {

		LexicalEnvironment currentLexicalEnvironment = lexicalEnvironment;

		while (!currentLexicalEnvironment.equals(LexicalEnvironment.NULL)) {

			final Marker marker = currentLexicalEnvironment.getMarker();
			if (Marker.BINDING_MARKERS.contains(marker)) {

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
