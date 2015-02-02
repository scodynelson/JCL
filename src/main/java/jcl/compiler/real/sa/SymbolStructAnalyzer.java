package jcl.compiler.real.sa;

import jcl.compiler.real.environment.Allocation;
import jcl.compiler.real.environment.Closure;
import jcl.compiler.real.environment.ClosureBinding;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.EnvironmentAccessor;
import jcl.compiler.real.environment.EnvironmentAllocation;
import jcl.compiler.real.environment.LocalAllocation;
import jcl.compiler.real.environment.Marker;
import jcl.compiler.real.environment.Scope;
import jcl.compiler.real.environment.SymbolBinding;
import jcl.compiler.real.environment.SymbolTable;
import jcl.compiler.real.sa.element.SymbolElement;
import jcl.symbols.SymbolStruct;
import jcl.types.T;
import org.springframework.stereotype.Component;

import java.util.Optional;
import java.util.Stack;

@Component
public class SymbolStructAnalyzer {

	public SymbolElement<?> analyzeLexicalSymbol(final SymbolStruct<?> input, final AnalysisBuilder analysisBuilder) {
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
			analyzeDynamicSymbol(input, analysisBuilder);
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

	public SymbolElement<?> analyzeDynamicSymbol(final SymbolStruct<?> input, final AnalysisBuilder analysisBuilder) {
		final Stack<Environment> environmentStack = analysisBuilder.getEnvironmentStack();

		final Environment environment = environmentStack.peek();

		final SymbolTable symbolTable = environment.getSymbolTable();
		final Optional<SymbolBinding> bindingInSymbolTable = symbolTable.getBinding(input);

		if (bindingInSymbolTable.isPresent()) {
			// Binding already exists in the current environment.
			return new SymbolElement<>(input);
		}

		final Environment currentEnclosingLambda = getEnclosingLambda(environment);

		if (environment.equals(currentEnclosingLambda)) {
			final int position = EnvironmentAccessor.getNextAvailableParameterNumber(environment);
			final Allocation allocation = new LocalAllocation(position);

			final SymbolBinding symbolBinding = new SymbolBinding(input, allocation, Scope.DYNAMIC, T.INSTANCE, Environment.FREE);
			symbolTable.addBinding(symbolBinding);

			return new SymbolElement<>(input);
		}

		// Add Binding to SymbolTable in the current environment
		final EnvironmentAllocation environmentAllocation = new EnvironmentAllocation(currentEnclosingLambda);
		final SymbolBinding symbolBinding = new SymbolBinding(input, environmentAllocation, Scope.DYNAMIC, T.INSTANCE, currentEnclosingLambda);
		symbolTable.addBinding(symbolBinding);

		final SymbolTable enclosingLambdaSymbolTable = currentEnclosingLambda.getSymbolTable();
		final Optional<SymbolBinding> enclosingSymbolBinding = enclosingLambdaSymbolTable.getBinding(input);

		if (enclosingSymbolBinding.isPresent()) {
			// Binding already exists in the Enclosing Lambda.
			return new SymbolElement<>(input);
		}

		// Add Binding to SymbolTable in the Enclosing Lambda.
		final int position = EnvironmentAccessor.getNextAvailableParameterNumber(environment);
		final Allocation allocation = new LocalAllocation(position);

		final SymbolBinding newSymbolBinding = new SymbolBinding(input, allocation, Scope.DYNAMIC, T.INSTANCE, Environment.FREE);
		enclosingLambdaSymbolTable.addBinding(newSymbolBinding);

		return new SymbolElement<>(input);
	}

	/**
	 * This method takes an environment and looks for the nearest enclosing lambda.
	 *
	 * @param environment
	 * 		The environment that is enclosed by a lambda
	 *
	 * @return The lambda enclosing the given environment.
	 */
	private static Environment getEnclosingLambda(final Environment environment) {

		Environment currentEnvironment = environment;

		final Marker marker = currentEnvironment.getMarker();
		while (!Marker.LAMBDA_MARKERS.contains(marker)) {
			currentEnvironment = currentEnvironment.getParent();
		}

		return currentEnvironment;
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
