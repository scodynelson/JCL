package jcl.compiler.real.sa;

import jcl.compiler.real.environment.Allocation;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.EnvironmentAccessor;
import jcl.compiler.real.environment.EnvironmentAllocation;
import jcl.compiler.real.environment.LocalAllocation;
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
public class DynamicSymbolStructAnalyzer extends SymbolStructAnalyzer {

	@Override
	public SymbolElement<?> analyzeSymbol(final SymbolStruct<?> input, final AnalysisBuilder analysisBuilder) {
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
}
