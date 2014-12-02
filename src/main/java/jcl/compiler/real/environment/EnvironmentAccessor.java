package jcl.compiler.real.environment;

import jcl.lists.ListStruct;
import jcl.symbols.SymbolStruct;

import java.util.ArrayList;
import java.util.List;

public class EnvironmentAccessor {

	public static boolean hasBinding(final Environment currentEnvironment, final SymbolStruct<?> variable) {
		return currentEnvironment.getBindings()
		                         .stream()
		                         .anyMatch(e -> e.getSymbolStruct().equals(variable));
	}

	public static Environment getBindingEnvironment(final Environment currentEnvironment, final SymbolStruct<?> variable,
	                                                final boolean valueBinding) {

		if (currentEnvironment.equals(Environment.NULL)) {
			return currentEnvironment;
		}

		if (hasBinding(currentEnvironment, variable)) {

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

		return getBindingEnvironment(currentEnvironment.getParent(), variable, valueBinding);
	}

	/**
	 * This method takes an environment and looks for the nearest enclosing lambda.
	 *
	 * @param currentEnvironment
	 * 		The environment that is enclosed by a lambda
	 *
	 * @return The lambda enclosing the given environment.
	 */
	public static Environment getEnclosingLambda(final Environment currentEnvironment) {

		if (currentEnvironment.equals(Environment.NULL)) {
			return currentEnvironment;
		}

		// if we are looking at a lambda, return it
		if (isLambda(currentEnvironment)) {
			return currentEnvironment;
		}

		// keep looking
		return getEnclosingLambda(currentEnvironment.getParent());
	}

	public static SymbolBinding getSymbolTableEntry(final Environment currentEnvironment, final SymbolStruct<?> variable) {

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

	public static SymbolBinding getSymbolInTable(final Environment currentEnvironment, final SymbolStruct<?> variable) {

		final SymbolTable symTable = currentEnvironment.getSymbolTable();
		final List<SymbolBinding> symbolBindings = symTable.getBindings();

		return symbolBindings
				.stream()
				.filter(e -> e.getSymbolStruct().equals(variable))
				.findFirst()
				.orElse(null);
	}

	public static Scope getSymbolScope(final Environment currentEnvironment, final SymbolStruct<?> variable) {

		// look up the symbol in the symbol table
		final SymbolBinding symPList = getSymbolTableEntry(currentEnvironment, variable);
		return symPList.getScope();
	}

	public static int getNextAvailableParameterNumber(final Environment environment) {
		return getNextAvailableParameterNumberRecur(environment, 0);
	}

	private static int getNextAvailableParameterNumberRecur(final Environment currentEnvironment, final int currentMax) {

		if (currentEnvironment.equals(Environment.NULL)) {
			return currentMax + 1;
		}

		final List<Binding> allEnvBindings = new ArrayList<>();

		// loop up through the local env we'return looking through
		allEnvBindings.addAll(currentEnvironment.getBindings());

		// now look through thru the symbol table (free variables)
		allEnvBindings.addAll(currentEnvironment.getSymbolTable().getBindings());

		final int envBindingsMax = allEnvBindings
				.stream()
				.map(Binding::getAllocation)
				.filter(e -> e instanceof PositionAllocation)
				.map(e -> (PositionAllocation) e)
				.mapToInt(PositionAllocation::getPosition)
				.max()
				.orElse(currentMax);
		final int newMax = Math.max(currentMax, envBindingsMax);

		// see if we just handled a lambda environment
		if (isLambda(currentEnvironment)) {
			// yup, all done
			return newMax + 1;
		}

		return getNextAvailableParameterNumberRecur(currentEnvironment.getParent(), newMax);
	}

	public static boolean isLambda(final Environment environment) {
		return (environment.getMarker() == Marker.LAMBDA) || (environment.getMarker() == Marker.FLET) || (environment.getMarker() == Marker.LABELS);
	}
}
