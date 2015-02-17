package jcl.compiler.real.environment;

import jcl.compiler.real.environment.allocation.LocalAllocation;
import jcl.compiler.real.environment.allocation.ParameterAllocation;
import jcl.compiler.real.environment.binding.EnvironmentParameterBinding;
import jcl.compiler.real.environment.binding.SymbolEnvironmentBinding;
import jcl.compiler.real.environment.binding.SymbolLocalBinding;
import jcl.symbols.SymbolStruct;

import java.util.List;
import java.util.Optional;

public class EnvironmentAccessor {

	public static int getNextAvailableParameterNumber(final Environment environment) {

		int currentMax = 0;

		Environment currentEnvironment = environment;
		Marker marker;

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

			// Need to see if we just got params from a lambda environment.
			marker = currentEnvironment.getMarker();

			currentEnvironment = currentEnvironment.getParent();
		} while (!Marker.LAMBDA_MARKERS.contains(marker));

		return currentMax + 1;
	}

	/*
	OLD BELOW
	 */

	public static Environment getBindingEnvironment(final Environment environment, final SymbolStruct<?> variable,
	                                                       final boolean valueBinding) {

		Environment currentEnvironment = environment;

		while (!currentEnvironment.equals(Environment.NULL)) {

			if (currentEnvironment.hasLexicalBinding(variable)) {

				final Marker envType = currentEnvironment.getMarker();
				if (((envType == Marker.LAMBDA) || (envType == Marker.LET)) && valueBinding) {
					return currentEnvironment;
				}

				if (((envType == Marker.FLET) || (envType == Marker.LABELS)) && !valueBinding) {
					return currentEnvironment;
				}

				if ((envType == Marker.MACROLET) && valueBinding) {
					return currentEnvironment;
				}
			}

			currentEnvironment = currentEnvironment.getParent();
		}

		return currentEnvironment;
	}

	public static Optional<SymbolLocalBinding> getSymbolTableEntry(final Environment currentEnvironment,
	                                                               final SymbolStruct<?> variable) {

		// look up the symbol in the symbol table
		final SymbolTable symTable = currentEnvironment.getSymbolTable();

		Optional<SymbolLocalBinding> symbolLocalBinding = symTable.getDynamicLocalBinding(variable);

		// if the cons starts with LOCAL, we're there
		// otherwise, we have to go to the actual env of allocation
		if (!symbolLocalBinding.isPresent()) {
			final Optional<SymbolEnvironmentBinding> symbolEnvironmentBinding = symTable.getDynamicEnvironmentBinding(variable);
			if (symbolEnvironmentBinding.isPresent()) {
				final SymbolEnvironmentBinding realSEB = symbolEnvironmentBinding.get();
				final SymbolTable sebSymbolTable = realSEB.getBinding().getSymbolTable();

				symbolLocalBinding = sebSymbolTable.getDynamicLocalBinding(variable);
			}
		}

		return symbolLocalBinding;
	}
}
