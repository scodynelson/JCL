package jcl.compiler.real.sa.analyzer;

import jcl.compiler.real.element.SymbolElement;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.EnvironmentAccessor;
import jcl.compiler.real.environment.EnvironmentStack;
import jcl.compiler.real.environment.Environments;
import jcl.compiler.real.environment.LambdaEnvironment;
import jcl.compiler.real.environment.Scope;
import jcl.compiler.real.environment.SymbolTable;
import jcl.compiler.real.environment.allocation.EnvironmentAllocation;
import jcl.compiler.real.environment.allocation.LocalAllocation;
import jcl.compiler.real.environment.binding.SymbolEnvironmentBinding;
import jcl.compiler.real.environment.binding.SymbolLocalBinding;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.symbols.SymbolStruct;
import jcl.types.T;
import org.springframework.stereotype.Component;

@Component
public class DynamicSymbolStructAnalyzer extends SymbolStructAnalyzer {

	private static final long serialVersionUID = 4236867001501188408L;

	@Override
	public SymbolElement<?> analyze(final SemanticAnalyzer analyzer, final SymbolStruct<?> input, final AnalysisBuilder analysisBuilder) {

		final EnvironmentStack environmentStack = analysisBuilder.getEnvironmentStack();
		final Environment currentEnvironment = environmentStack.peek();

		final SymbolTable currentEnvironmentSymbolTable = currentEnvironment.getSymbolTable();
		final boolean hasSymbolBinding = currentEnvironmentSymbolTable.hasBinding(input);

		if (hasSymbolBinding) {
			// Binding already exists in the current environment.
			return new SymbolElement<>(input);
		}

		final LambdaEnvironment currentEnclosingLambda = Environments.getEnclosingLambda(currentEnvironment);

		if (currentEnvironment.equals(currentEnclosingLambda)) {
			final int position = EnvironmentAccessor.getNextAvailableParameterNumber(currentEnvironment);
			final LocalAllocation allocation = new LocalAllocation(position);

			// TODO: get rid of Scope
			final SymbolLocalBinding symbolBinding
					= new SymbolLocalBinding(input, allocation, Scope.DYNAMIC, T.INSTANCE, currentEnvironment);
			currentEnvironmentSymbolTable.addLocalBinding(symbolBinding);

			return new SymbolElement<>(input);
		}

		// Add Binding to SymbolTable in the current environment
		final EnvironmentAllocation environmentAllocation = new EnvironmentAllocation(currentEnclosingLambda);
		// TODO: get rid of Scope
		final SymbolEnvironmentBinding symbolBinding
				= new SymbolEnvironmentBinding(input, environmentAllocation, Scope.DYNAMIC, T.INSTANCE, currentEnclosingLambda);
		currentEnvironmentSymbolTable.addEnvironmentBinding(symbolBinding);

		final SymbolTable enclosingLambdaSymbolTable = currentEnclosingLambda.getSymbolTable();
		final boolean enclosingLambdaHasSymbolBinding = enclosingLambdaSymbolTable.hasBinding(input);

		if (enclosingLambdaHasSymbolBinding) {
			// Binding already exists in the Enclosing Lambda.
			return new SymbolElement<>(input);
		}

		// Add Binding to SymbolTable in the Enclosing Lambda.
		final int position = EnvironmentAccessor.getNextAvailableParameterNumber(currentEnvironment);
		final LocalAllocation allocation = new LocalAllocation(position);

		// TODO: get rid of Scope
		final SymbolLocalBinding newSymbolBinding
				= new SymbolLocalBinding(input, allocation, Scope.DYNAMIC, T.INSTANCE, currentEnclosingLambda);
		enclosingLambdaSymbolTable.addLocalBinding(newSymbolBinding);

		return new SymbolElement<>(input);
	}
}
