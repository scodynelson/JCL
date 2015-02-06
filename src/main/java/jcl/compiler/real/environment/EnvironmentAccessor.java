package jcl.compiler.real.environment;

import jcl.compiler.real.environment.allocation.LocalAllocation;
import jcl.compiler.real.environment.allocation.ParameterAllocation;
import jcl.compiler.real.environment.binding.EnvironmentBinding;
import jcl.compiler.real.environment.binding.SymbolEnvironmentBinding;
import jcl.compiler.real.environment.binding.SymbolLocalBinding;
import jcl.symbols.SymbolStruct;

import java.util.List;
import java.util.Optional;
import java.util.Set;

public class EnvironmentAccessor {

	public static LexicalEnvironment getBindingEnvironment(final SymbolStruct<?> variable, final LexicalEnvironment lexicalEnvironment,
	                                                       final Set<Marker> lexicalEnvironmentTypes) {

		LexicalEnvironment currentLexicalEnvironment = lexicalEnvironment;

		while (!currentLexicalEnvironment.equals(LexicalEnvironment.NULL)) {

			final Marker lexicalEnvironmentType = currentLexicalEnvironment.getMarker();
			if (lexicalEnvironmentTypes.contains(lexicalEnvironmentType)) {

				final boolean hasBinding = currentLexicalEnvironment.hasBinding(variable);
				if (hasBinding) {
					break;
				}
			}

			currentLexicalEnvironment = currentLexicalEnvironment.getParent();
		}

		return currentLexicalEnvironment;
	}

	public static int getNextAvailableParameterNumber(final LexicalEnvironment environment) {

		int currentMax = 0;

		LexicalEnvironment currentEnvironment = environment;
		Marker marker;

		do {
			// loop up through the local env we're turn looking through
			final List<EnvironmentBinding> environmentBindings = currentEnvironment.getBindings();

			int envBindingsMax = environmentBindings
					.stream()
					.map(EnvironmentBinding::getAllocation)
					.mapToInt(ParameterAllocation::getPosition)
					.max()
					.orElse(currentMax);
			currentMax = Math.max(currentMax, envBindingsMax);

			// now look through through the symbol table (free variables)
			final List<SymbolLocalBinding> symbolBindings = currentEnvironment.getSymbolTable().getLocalBindings();

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

	public static LexicalEnvironment getBindingEnvironment(final LexicalEnvironment environment, final SymbolStruct<?> variable,
	                                                       final boolean valueBinding) {

		LexicalEnvironment currentEnvironment = environment;

		while (!currentEnvironment.equals(LexicalEnvironment.NULL)) {

			if (currentEnvironment.hasBinding(variable)) {

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

	public static Optional<SymbolLocalBinding> getSymbolTableEntry(final LexicalEnvironment currentEnvironment,
	                                                               final SymbolStruct<?> variable) {

		// look up the symbol in the symbol table
		final SymbolTable symTable = currentEnvironment.getSymbolTable();

		Optional<SymbolLocalBinding> symbolLocalBinding = symTable.getLocalBinding(variable);

		// if the cons starts with LOCAL, we're there
		// otherwise, we have to go to the actual env of allocation
		if (!symbolLocalBinding.isPresent()) {
			final Optional<SymbolEnvironmentBinding> symbolEnvironmentBinding = symTable.getEnvironmentBinding(variable);
			if (symbolEnvironmentBinding.isPresent()) {
				final SymbolEnvironmentBinding realSEB = symbolEnvironmentBinding.get();
				final SymbolTable sebSymbolTable = realSEB.getBinding().getSymbolTable();

				symbolLocalBinding = sebSymbolTable.getLocalBinding(variable);
			}
		}

		return symbolLocalBinding;
	}
}
