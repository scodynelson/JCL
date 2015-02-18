package jcl.compiler.real.environment;

import jcl.compiler.real.environment.allocation.LocalAllocation;
import jcl.compiler.real.environment.allocation.ParameterAllocation;
import jcl.compiler.real.environment.binding.EnvironmentParameterBinding;
import jcl.compiler.real.environment.binding.SymbolLocalBinding;

import java.util.List;

public class EnvironmentAccessor {

	public static int getNextAvailableParameterNumber(final Environment environment) {

		int currentMax = 0;

		Environment currentEnvironment = environment;

		do {
			// loop up through the local env we're turn looking through
			final List<EnvironmentParameterBinding> environmentBindings = currentEnvironment.getLexicalBindings();

			int envBindingsMax = environmentBindings
					.stream()
					.map(EnvironmentParameterBinding::getAllocation)
					.mapToInt(ParameterAllocation::getPosition)
					.max()
					.orElse(currentMax);
			currentMax = Math.max(currentMax, envBindingsMax);

			// now look through through the symbol table (free variables)
			final List<SymbolLocalBinding> symbolBindings = currentEnvironment.getSymbolTable().getDynamicLocalBindings();

			envBindingsMax = symbolBindings
					.stream()
					.map(SymbolLocalBinding::getAllocation)
					.mapToInt(LocalAllocation::getPosition)
					.max()
					.orElse(currentMax);
			currentMax = Math.max(currentMax, envBindingsMax);

			currentEnvironment = currentEnvironment.getParent();
		} while (!(currentEnvironment instanceof LambdaEnvironment));

		return currentMax + 1;
	}
}
