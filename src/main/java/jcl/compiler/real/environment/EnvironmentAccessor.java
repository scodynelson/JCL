package jcl.compiler.real.environment;

import jcl.symbols.SymbolStruct;

import java.util.ArrayList;
import java.util.List;

public class EnvironmentAccessor {

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

	public static SymbolBinding getSymbolTableEntry(final LexicalEnvironment currentEnvironment, final SymbolStruct<?> variable) {

		// look up the symbol in the symbol table
		SymbolBinding symPList = getSymbolInTable(currentEnvironment, variable);

		// (:ALLOCATION (:LOCAL . n) :BINDING :FREE :SCOPE :DYNAMIC :TYPE T)
		// we need the local slot in the allocation, get the CDR of the GET of :ALLOCATION
		final Allocation alloc = symPList.getAllocation();

		// if the cons starts with LOCAL, we're there
		// otherwise, we have to go to the actual env of allocation
		if (!(alloc instanceof LocalAllocation)) {
			symPList = getSymbolInTable(symPList.getBinding(), variable);
		}

		return symPList;
	}

	private static SymbolBinding getSymbolInTable(final Environment<?> currentEnvironment, final SymbolStruct<?> variable) {

		final SymbolTable symTable = currentEnvironment.getSymbolTable();
		final List<SymbolBinding> symbolBindings = symTable.getBindings();

		return symbolBindings
				.stream()
				.filter(e -> e.getSymbolStruct().equals(variable))
				.findFirst()
				.orElse(null);
	}

	public static int getNextAvailableParameterNumber(final LexicalEnvironment environment) {
		int currentMax = 0;

		LexicalEnvironment currentEnvironment = environment;
		while (!currentEnvironment.equals(LexicalEnvironment.NULL)) {

			final List<Binding> allEnvironmentBindings = new ArrayList<>();

			// loop up through the local env we're turn looking through
			allEnvironmentBindings.addAll(currentEnvironment.getBindings());

			// now look through through the symbol table (free variables)
			allEnvironmentBindings.addAll(currentEnvironment.getSymbolTable().getBindings());

			final int envBindingsMax = allEnvironmentBindings
					.stream()
					.map(Binding::getAllocation)
					.filter(e -> e instanceof PositionAllocation)
					.map(e -> (PositionAllocation) e)
					.mapToInt(PositionAllocation::getPosition)
					.max()
					.orElse(currentMax);
			currentMax = Math.max(currentMax, envBindingsMax);

			// see if we just handled a lambda environment
			final Marker marker = currentEnvironment.getMarker();
			if (Marker.LAMBDA_MARKERS.contains(marker)) {
				// yup, all done
				break;
			}

			currentEnvironment = currentEnvironment.getParent();
		}

		return currentMax + 1;
	}
}
