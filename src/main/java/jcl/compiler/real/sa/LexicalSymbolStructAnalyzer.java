package jcl.compiler.real.sa;

import jcl.compiler.real.environment.Closure;
import jcl.compiler.real.environment.ClosureBinding;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.EnvironmentAllocation;
import jcl.compiler.real.environment.Marker;
import jcl.compiler.real.environment.Scope;
import jcl.compiler.real.environment.SymbolBinding;
import jcl.compiler.real.environment.SymbolTable;
import jcl.compiler.real.sa.element.SymbolElement;
import jcl.symbols.SymbolStruct;
import jcl.types.T;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Optional;
import java.util.Stack;

@Component
public class LexicalSymbolStructAnalyzer extends SymbolStructAnalyzer {

	@Autowired
	private DynamicSymbolStructAnalyzer dynamicSymbolStructAnalyzer;

	@Override
	public SymbolElement<?> analyzeSymbol(final SymbolStruct<?> input, final AnalysisBuilder analysisBuilder) {
		final Stack<Environment> environmentStack = analysisBuilder.getEnvironmentStack();

		final Environment environment = environmentStack.peek();

		final boolean symbolBoundInCurrentEnvironment = environment.hasBinding(input);
		if (symbolBoundInCurrentEnvironment) {
			// Binding already exists in the current environment.
			return new SymbolElement<>(input);
		}

		final Environment bindingEnvironment = getBindingEnvironment(environment, input);

		if (bindingEnvironment.equals(Environment.NULL)) {
			// No inner binding environments. Add it as a DYNAMIC symbol in the current environment before we proceed.
			dynamicSymbolStructAnalyzer.analyzeSymbol(input, analysisBuilder);
		}

		final Environment currentEnclosingLambda = getEnclosingLambda(environment);
		final Environment bindingEnclosingLambda = getEnclosingLambda(bindingEnvironment);

		final SymbolTable currentEnvironmentSymbolTable = environment.getSymbolTable();

		if (currentEnclosingLambda.equals(bindingEnclosingLambda)) {
			// Binding Lambda and Enclosing Lambda are the same. No need for a Closure.

			// Create a new SymbolBinding and reference it to the 'bindingEnvironment'
			final EnvironmentAllocation allocation = new EnvironmentAllocation(bindingEnclosingLambda);
			final SymbolBinding symbolBinding = new SymbolBinding(input, allocation, Scope.LEXICAL, T.INSTANCE, bindingEnvironment);

			// Now add that new symbol to the SymbolTable of the 'currentEnvironment'
			currentEnvironmentSymbolTable.addBinding(symbolBinding);
			return new SymbolElement<>(input);
		}

		// Here the Binding Lambda is outside of the Enclosing Lambda
		final Environment outerBindingEnvironment = getBindingEnvironment(bindingEnvironment, input);

		if (outerBindingEnvironment.equals(Environment.NULL)) {
			// Outer Binding Environment is the NULL Environment. Therefore, we can't create a Closure.

			// Create a new SymbolBinding and reference it to the 'bindingEnvironment'
			final EnvironmentAllocation allocation = new EnvironmentAllocation(Environment.NULL);
			final SymbolBinding symbolBinding = new SymbolBinding(input, allocation, Scope.LEXICAL, T.INSTANCE, bindingEnvironment);

			// Now add that new symbol to the SymbolTable of the 'currentEnvironment'
			currentEnvironmentSymbolTable.addBinding(symbolBinding);
			return new SymbolElement<>(input);
		}

		// There is an Outer Binding Environment. Therefore, we will create a Closure Binding in that Environment.
		final Closure closure = outerBindingEnvironment.getEnvironmentClosure();

		final Optional<ClosureBinding> closureBinding = closure.getBinding(input);

		if (closureBinding.isPresent()) {
			// Closure Binding already exists in the Outer Binding Environment.

			final ClosureBinding closureBindingValue = closureBinding.get();

			// Increment the number of references to this Closure Binding.
			closureBindingValue.incrementReferences();
		} else {
			// Create a new ClosureBinding in the Outer Binding Environment.
			final ClosureBinding newClosureBinding = new ClosureBinding(input, closure.getBindings().size(), 1);
			closure.addBinding(newClosureBinding);
		}

		return new SymbolElement<>(input);
	}

	private static Environment getBindingEnvironment(final Environment environment, final SymbolStruct<?> variable) {

		Environment currentEnvironment = environment;

		while (!currentEnvironment.equals(Environment.NULL)) {

			final Marker marker = currentEnvironment.getMarker();
			if (Marker.BINDING_MARKERS.contains(marker)) {

				final boolean hasBinding = currentEnvironment.hasBinding(variable);
				if (hasBinding) {
					break;
				}
			}

			currentEnvironment = currentEnvironment.getParent();
		}

		return currentEnvironment;
	}
}
