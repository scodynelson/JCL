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

	public SymbolElement<?> analyze(final SymbolStruct<?> input, final AnalysisBuilder analysisBuilder, final boolean isSpecial) {
		final Stack<Environment> environmentStack = analysisBuilder.getEnvironmentStack();

		final Environment environment = environmentStack.peek();
		if (isSpecial) {
			addDynamicSymbolToEnvironment(environment, input);
		} else {
			addLexicalSymbolToEnvironment(environment, input);
		}

		return new SymbolElement<>(input);
	}

	private static void addDynamicSymbolToEnvironment(final Environment currentEnvironment, final SymbolStruct<?> newSymbol) {

		final SymbolTable symbolTable = currentEnvironment.getSymbolTable();
		final Optional<SymbolBinding> bindingInSymbolTable = symbolTable.getBinding(newSymbol);

		if (bindingInSymbolTable.isPresent()) {
			// Binding already exists in the current environment.
			return;
		}

		final Environment currentEnclosingLambda = getEnclosingLambda(currentEnvironment);

		if (currentEnvironment.equals(currentEnclosingLambda)) {
			final int position = EnvironmentAccessor.getNextAvailableParameterNumber(currentEnvironment);
			final Allocation allocation = new LocalAllocation(position);

			final SymbolBinding symbolBinding = new SymbolBinding(newSymbol, allocation, Scope.DYNAMIC, T.INSTANCE, Environment.FREE);
			symbolTable.addBinding(symbolBinding);

			return;
		}

		// Add Binding to SymbolTable in the current environment
		final EnvironmentAllocation environmentAllocation = new EnvironmentAllocation(currentEnclosingLambda);
		final SymbolBinding symbolBinding = new SymbolBinding(newSymbol, environmentAllocation, Scope.DYNAMIC, T.INSTANCE, currentEnclosingLambda);
		symbolTable.addBinding(symbolBinding);

		final SymbolTable enclosingLambdaSymbolTable = currentEnclosingLambda.getSymbolTable();
		final Optional<SymbolBinding> enclosingSymbolBinding = enclosingLambdaSymbolTable.getBinding(newSymbol);

		if (enclosingSymbolBinding.isPresent()) {
			// Binding already exists in the Enclosing Lambda.
			return;
		}

		// Add Binding to SymbolTable in the Enclosing Lambda.
		final int position = EnvironmentAccessor.getNextAvailableParameterNumber(currentEnvironment);
		final Allocation allocation = new LocalAllocation(position);

		final SymbolBinding newSymbolBinding = new SymbolBinding(newSymbol, allocation, Scope.DYNAMIC, T.INSTANCE, Environment.FREE);
		enclosingLambdaSymbolTable.addBinding(newSymbolBinding);
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

	private static void addLexicalSymbolToEnvironment(final Environment currentEnvironment, final SymbolStruct<?> newSymbol) {

		final boolean symbolBoundInCurrentEnvironment = currentEnvironment.hasBinding(newSymbol);
		if (symbolBoundInCurrentEnvironment) {
			// Binding already exists in the current environment.
			return;
		}

		final Environment bindingEnvironment = getBindingEnvironment(currentEnvironment, newSymbol);

		if (bindingEnvironment.equals(Environment.NULL)) {
			// No inner binding environments. Add it as a DYNAMIC symbol in the current environment before we proceed.
			addDynamicSymbolToEnvironment(currentEnvironment, newSymbol);
		}

		final Environment currentEnclosingLambda = getEnclosingLambda(currentEnvironment);
		final Environment bindingEnclosingLambda = getEnclosingLambda(bindingEnvironment);

		final SymbolTable currentEnvironmentSymbolTable = currentEnvironment.getSymbolTable();

		if (currentEnclosingLambda.equals(bindingEnclosingLambda)) {
			// Binding Lambda and Enclosing Lambda are the same. No need for a Closure.

			// Create a new SymbolBinding and reference it to the 'bindingEnvironment'
			final EnvironmentAllocation allocation = new EnvironmentAllocation(bindingEnclosingLambda);
			final SymbolBinding symbolBinding = new SymbolBinding(newSymbol, allocation, Scope.LEXICAL, T.INSTANCE, bindingEnvironment);

			// Now add that new symbol to the SymbolTable of the 'currentEnvironment'
			currentEnvironmentSymbolTable.addBinding(symbolBinding);
			return;
		}

		// Here the Binding Lambda is outside of the Enclosing Lambda
		final Environment outerBindingEnvironment = getBindingEnvironment(bindingEnvironment, newSymbol);

		if (outerBindingEnvironment.equals(Environment.NULL)) {
			// Outer Binding Environment is the NULL Environment. Therefore, we can't create a Closure.

			// Create a new SymbolBinding and reference it to the 'bindingEnvironment'
			final EnvironmentAllocation allocation = new EnvironmentAllocation(Environment.NULL);
			final SymbolBinding symbolBinding = new SymbolBinding(newSymbol, allocation, Scope.LEXICAL, T.INSTANCE, bindingEnvironment);

			// Now add that new symbol to the SymbolTable of the 'currentEnvironment'
			currentEnvironmentSymbolTable.addBinding(symbolBinding);
			return;
		}

		// There is an Outer Binding Environment. Therefore, we will create a Closure Binding in that Environment.
		final Closure closure = outerBindingEnvironment.getEnvironmentClosure();

		final Optional<ClosureBinding> closureBinding = closure.getBinding(newSymbol);

		if (closureBinding.isPresent()) {
			// Closure Binding already exists in the Outer Binding Environment.

			final ClosureBinding closureBindingValue = closureBinding.get();

			// Increment the number of references to this Closure Binding.
			closureBindingValue.incrementReferences();
		} else {
			// Create a new ClosureBinding in the Outer Binding Environment.
			final ClosureBinding newClosureBinding = new ClosureBinding(newSymbol, closure.getBindings().size(), 1);
			closure.addBinding(newClosureBinding);
		}
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
