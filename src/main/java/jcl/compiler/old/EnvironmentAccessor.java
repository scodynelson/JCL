package jcl.compiler.old;

import jcl.LispStruct;
import jcl.compiler.old.symbol.KeywordOld;
import jcl.compiler.real.environment.Allocation;
import jcl.compiler.real.environment.Binding;
import jcl.compiler.real.environment.Closure;
import jcl.compiler.real.environment.ClosureBinding;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.LambdaBinding;
import jcl.compiler.real.environment.LetBinding;
import jcl.compiler.real.environment.LocalAllocation;
import jcl.compiler.real.environment.MacroFunctionBinding;
import jcl.compiler.real.environment.Marker;
import jcl.compiler.real.environment.PositionAllocation;
import jcl.compiler.real.environment.Scope;
import jcl.compiler.real.environment.SymbolBinding;
import jcl.compiler.real.environment.SymbolTable;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.symbols.SymbolStruct;
import jcl.symbols.TStruct;
import jcl.types.T;

import java.util.List;

public class EnvironmentAccessor {

	public static Environment createNewEnvironment(final Marker marker) {
		Environment environment = new Environment(marker, null, null, null, null, null);

		if ((marker == Marker.LAMBDA) || (marker == Marker.MACRO) || (marker == Marker.FLET) || (marker == Marker.LABELS)) {
			environment.setLoadTimeValue(null);
		}

		environment = createParent(environment, Environment.NULL);
		return environment;
	}

	public static Environment createParent(final Environment currentEnvironment, final Environment parent) {
		currentEnvironment.setParent(parent);
		return currentEnvironment;
	}

	public static Environment createGlobalEnvironment() {
		final Environment newEnvironment = createNewEnvironment(Marker.LAMBDA);
		return createParent(newEnvironment, Environment.NULL);
	}

	public static Environment getParent(final Environment currentEnvironment) {
		return currentEnvironment.getParent();
	}

	public static Environment createNewLambdaBinding(final Environment currentEnvironment, final SymbolStruct newVariable,
													 final int position, final boolean isSpecial) {

		final Scope scope = (newVariable.isSpecial() || isSpecial) ? Scope.DYNAMIC : Scope.LEXICAL;
		final Binding binding = new LambdaBinding(newVariable, position, scope, T.INSTANCE);

		final List<Binding> currentBindings = currentEnvironment.getBindings();
		currentBindings.add(binding);

		return currentEnvironment;
	}

	public static Environment createNewLetBinding(final Environment currentEnvironment, final SymbolStruct newVariable,
												  final int position, final LispStruct initForm, final boolean isSpecial) {

		final Scope scope = (newVariable.isSpecial() || isSpecial) ? Scope.DYNAMIC : Scope.LEXICAL;
		final Binding binding = new LetBinding(newVariable, position, scope, T.INSTANCE, initForm);

		final List<Binding> currentBindings = currentEnvironment.getBindings();
		currentBindings.add(binding);

		return currentEnvironment;
	}

	public static Environment createNewFBinding(final Environment currentEnvironment, final SymbolStruct newVariable,
												final int position, final SymbolStruct newFieldName, final boolean isSpecial) {

		final Scope scope = (newVariable.isSpecial() || isSpecial) ? Scope.DYNAMIC : Scope.LEXICAL;
		final Binding binding = new MacroFunctionBinding(newVariable, position, scope, T.INSTANCE, newFieldName);

		final List<Binding> currentBindings = currentEnvironment.getBindings();
		currentBindings.add(binding);

		return currentEnvironment;
	}

	public static List<Binding> getBindingSet(final Environment currentEnvironment) {
		return currentEnvironment.getBindings();
	}

	public static Closure getClosureSet(final Environment currentEnvironment) {
		return currentEnvironment.getEnvironmentClosure();
	}

	public static Binding getBinding(final Environment currentEnvironment, final SymbolStruct variable) {
		final List<Binding> bindings = currentEnvironment.getBindings();

		Binding returnBinding = null;
		for (final Binding binding : bindings) {
			if (binding.getSymbolStruct().equals(variable)) {
				returnBinding = binding;
				break;
			}
		}
		return returnBinding;
	}

	public static SymbolStruct extractBoundName(final Environment env, final SymbolStruct fnName, final boolean valueBinding) {

		// get the current environment - used for macroexpansion
		final Environment fnBinding = getBindingEnvironment(env, fnName, valueBinding);
		if (fnBinding.equals(Environment.NULL)) {
			return fnName;
		}

		// use assoc to get bindings
		final List<Binding> aBindings = fnBinding.getBindings();

		Binding foundBinding = null;
		for (final Binding binding : aBindings) {
			if (binding.getSymbolStruct().equals(fnName)) {
				foundBinding = binding;
				break;
			}
		}

		if (foundBinding != null) {
			return ((MacroFunctionBinding) foundBinding).getName();
		}
		return null;
	}

	public static boolean hasBinding(final Environment currentEnvironment, final SymbolStruct variable) {
		final List<Binding> bindings = currentEnvironment.getBindings();

		boolean hasBinding = false;
		for (final Binding binding : bindings) {
			if (binding.getSymbolStruct().equals(variable)) {
				hasBinding = true;
				break;
			}
		}
		return hasBinding;
	}

	public static Environment getBindingEnvironment(final Environment currentEnvironment, final SymbolStruct variable,
													final boolean valueBinding) {

		if (currentEnvironment.equals(Environment.NULL)) {
			return currentEnvironment;
		}

		if (hasBinding(currentEnvironment, variable)) {

			final Marker envType = currentEnvironment.getMarker();
			if (((envType == Marker.LAMBDA) || (envType == Marker.MACRO) || (envType == Marker.LET)) && valueBinding) {
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

	public static Environment findClosestLambdaOrLetEnv(final Environment currentEnvironment) {

		if (currentEnvironment.equals(Environment.NULL)) {
			return currentEnvironment;
		}

		final Marker envType = currentEnvironment.getMarker();
		if ((envType == Marker.LAMBDA) || (envType == Marker.MACRO) || (envType == Marker.LET) || (envType == Marker.LABELS) || (envType == Marker.FLET)) {
			return currentEnvironment;
		}

		return findClosestLambdaOrLetEnv(getParent(currentEnvironment));
	}

	public static Environment addSymbolToTable(final Environment currentEnvironment, final SymbolStruct newSymbol) {
		final Environment currentEnvironmentInner = findClosestLambdaOrLetEnv(currentEnvironment);

		/* default - may change later */
		ListStruct pList = new ConsStruct(KeywordOld.Type, ListStruct.buildProperList(TStruct.INSTANCE));

		// ...(:TYPE T)
		// if this symbol is marked as special, it automatically goes to the second section
		if (!newSymbol.isSpecial()) {

			// This section deals with the lexical binding of symbols that are bound above the current env
			Environment bindingEnvironment = getBindingEnvironment(currentEnvironmentInner, newSymbol, true);
			if (!bindingEnvironment.equals(Environment.NULL)) {

				// so it has a lexical binding somewhere
				if (getBinding(currentEnvironmentInner, newSymbol) == null) {

					// ...(:BINDING #the-binding-env# :SCOPE :LEXICAL :TYPE T)
					// so we just have to point the allocation at the binding environment
					//*** here is where we know that there is a closure involved ***
					// See if it is bound in the current lambda env. If so, just carry on
					final Environment currentLambda = getEnclosingLambda(currentEnvironmentInner);

					// is the binding location above the current lambda? If so, there has
					// to be a closure allocation in the bindingEnvironment
					final Environment bindingLambda = getEnclosingLambda(bindingEnvironment);
					if (!currentLambda.equals(bindingLambda)) {

						// here the binding lambda is outside the enclosing lambda
						// this calls for a closure allocation in the outer env (lambda or let)
						addClosureToBindingEnvironment(bindingEnvironment, newSymbol);

						// the (:allocation (:closure . bindingEnv)) to the table
//						bindingEnvironment = new ConsStruct(KeywordOld.Closure, bindingEnvironment);
						// TODO: Closure allocation???
					}

					final SymbolBinding symbolBinding = new SymbolBinding(newSymbol, null, Scope.LEXICAL, T.INSTANCE, bindingEnvironment);
					currentEnvironmentInner.getSymbolTable().getBindings().add(symbolBinding);
				}

				// we may also be here because there is a binding right here. Nothing to do
				return currentEnvironmentInner;
			}
		}


		// if not bound anywhere in the binding tree (free and dynamic)...
		// we know we're dealing with dynamic variables - so, is it already tagged in this environment?
		if (getSymbolInTable(currentEnvironmentInner, newSymbol) != null) {
			// yes, nothing to do
			return currentEnvironmentInner;
		}

		// so, we at least have to add an entry in this environment
		// and possibly in an outer one

		// ...(:BINDING :FREE :SCOPE :DYNAMIC :TYPE T)

		final Allocation finalAllocation;
		final Environment finalBindingEnvironment;
		// if the current environment is not a lambda, we may need to add an entry to the enclosing lambda
		if (isLambda(currentEnvironmentInner)) {

			// symbol was found in the immediate scope of the Lambda
			finalAllocation = addLocalAlloc(currentEnvironmentInner);
			finalBindingEnvironment = Environment.FREE;

			// ...(:ALLOCATION (:LOCAL . n) :BINDING :FREE :SCOPE :DYNAMIC :TYPE T)
		} else {

			// if the current environment is not a lambda, find the enclosing lambda
			final Environment enclosingLambda = getEnclosingLambda(currentEnvironmentInner);

			// see if the symbol is already in the enclosing environment
			if (getSymbolInTable(enclosingLambda, newSymbol) == null) {

				// it's not in the enclosing lambda, so put in an entry
				final Allocation enclosingSymTbl = addLocalAlloc(currentEnvironmentInner);

				// add to the enclosing lambda's symbol table
				final SymbolBinding symbolBinding = new SymbolBinding(newSymbol, enclosingSymTbl, Scope.DYNAMIC, T.INSTANCE, Environment.FREE);
				enclosingLambda.getSymbolTable().getBindings().add(symbolBinding);

				// Now we have the outer symbol table set up correctly
				// set the Allocation KeywordOld to what is should be for the current environment
				finalAllocation = null;
				finalBindingEnvironment = enclosingLambda;

				// ...(:ALLOCATION #enclosing-lambda# :BINDING :FREE :SCOPE :DYNAMIC :TYPE T)
			} else {
				// it is, so we have to add a reference to that environment in the current env

				final SymbolBinding symbolBinding = new SymbolBinding(newSymbol, null, Scope.DYNAMIC, T.INSTANCE, enclosingLambda);
				currentEnvironmentInner.getSymbolTable().getBindings().add(symbolBinding);
				return currentEnvironmentInner;
			}
		}

		// now we add the new symbol to the local table
		final SymbolBinding symbolBinding = new SymbolBinding(newSymbol, finalAllocation, Scope.DYNAMIC, T.INSTANCE, finalBindingEnvironment);
		currentEnvironmentInner.getSymbolTable().getBindings().add(symbolBinding);
		return currentEnvironmentInner;
	}

	public static Allocation addLocalAlloc(final Environment currentEnvironment) {
		final int localNumber = getNextAvailableParameterNumber(currentEnvironment);
		return new LocalAllocation(localNumber);
	}

	/**
	 * This method takes an environment and looks for the nearest enclosing lambda.
	 *
	 * @param environment The environment that is enclosed by a lambda
	 * @return The lambda enclosing the given environment.
	 */
	public static Environment getEnclosingLambda(final Environment environment) {
		// if we are looking at a lambda, return it
		if (isLambda(environment)) {
			return environment;
		}

		final Environment parent = getParent(environment);

		// if the parent is null and there is no enclosing lambda, return NIL
		if (parent.equals(Environment.NULL)) {
			return Environment.NULL;
		}

		// keep looking
		return getEnclosingLambda(parent);
	}

	public static SymbolTable getSymbolTable(final Environment currentEnvironment) {
		final Environment currentEnvironmentInner = findClosestLambdaOrLetEnv(currentEnvironment);
		return currentEnvironmentInner.getSymbolTable();
	}

	public static SymbolBinding getSymbolInTable(final Environment currentEnvironment, final SymbolStruct variable) {
		final Environment currentEnvironmentInner = findClosestLambdaOrLetEnv(currentEnvironment);

		final SymbolTable symTable = getSymbolTable(currentEnvironmentInner);
		final List<Binding> symbolBindings = symTable.getBindings();

		SymbolBinding returnSymbolBinding = null;
		for (final Binding symbolBinding : symbolBindings) {
			if (symbolBinding.getSymbolStruct().equals(variable)) {
				returnSymbolBinding = (SymbolBinding) symbolBinding;
				break;
			}
		}
		return returnSymbolBinding;
	}

	public static SymbolBinding getSymbolTableEntry(final Environment currentEnvironment, final SymbolStruct variable) {
		final Environment currentEnvironmentInner = findClosestLambdaOrLetEnv(currentEnvironment);

		// look up the symbol in the symbol table
		SymbolBinding symPList = getSymbolInTable(currentEnvironmentInner, variable);

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

	public static int getSymbolAllocation(final Environment currentEnvironment, final SymbolStruct variable) {
		final Environment currentEnvironmentInner = findClosestLambdaOrLetEnv(currentEnvironment);

		// look up the symbol in the symbol table
		final SymbolBinding symPList = getSymbolTableEntry(currentEnvironmentInner, variable);
//        List symPList = getSymbolInTable(currentEnvironmentInner, variable);

//        // (:ALLOCATION (:LOCAL . n) :BINDING :FREE :SCOPE :DYNAMIC :TYPE T)
//        // we need the local slot in the allocation, get the CDR of the GET of :ALLOCATION

		final Allocation alloc = symPList.getAllocation();

//        // if the cons starts with LOCAL, we're there
//        // otherwise, we have to go to the actual env of allocation
//        if (alloc.getFirst() != KeywordOld.Local) {
//            symPList = getSymbolInTable(alloc, variable);
		if (alloc == null) {
			return -1;
		} else {
			return ((PositionAllocation) alloc).getPosition();
		}
	}

	public static Scope getSymbolScope(final Environment currentEnvironment, final SymbolStruct variable) {
		final Environment currentEnvironmentInner = findClosestLambdaOrLetEnv(currentEnvironment);

		// look up the symbol in the symbol table
		final SymbolBinding symPList = getSymbolTableEntry(currentEnvironmentInner, variable);
		return symPList.getScope();
	}

	public static int getLocalMax(final List<Binding> bindings, int currentMax) {

		int currentMaxInner = currentMax;

		for (final Binding binding : bindings) {
			final Allocation allocation = binding.getAllocation();

			// an allocation is a ref to an outer env, (:local . n) or (:parameter . n)
			if (allocation instanceof PositionAllocation) {

				PositionAllocation positionAllocation = (PositionAllocation) allocation;
				final int tempParameter = positionAllocation.getPosition();
				if (tempParameter > currentMaxInner) {
					currentMaxInner = tempParameter;
				}
			}
		}

		return currentMaxInner;
	}

	public static int getNextAvailableParameterNumber(final Environment environment) {
		Environment environmentInner = environment;

		// starts with the current environment
		int max = 0;
		ListStruct bindings;

		// Loop through the envs until you hit a lambda (inclusive)
		while (true) {

			// loop up through the local env we'return looking through
			max = getLocalMax(environmentInner.getBindings(), max);

			// now look through thru the symbol table (free variables)
			max = getLocalMax(environmentInner.getSymbolTable().getBindings(), max);

			// see if we just handled a lambda environment
			if (isLambda(environmentInner)) {
				// yup, all done
				break;
			}

			environmentInner = getParent(environmentInner);
			assert !environmentInner.equals(Environment.NULL);
		}

		// return one more than the max
		return max + 1;
	}

	public static boolean isLambda(final Environment environment) {
		return (environment.getMarker() == Marker.LAMBDA) || (environment.getMarker() == Marker.MACRO)
				|| (environment.getMarker() == Marker.FLET) || (environment.getMarker() == Marker.LABELS);
	}

	public static Environment addClosureToBindingEnvironment(final Environment currentEnvironment, final SymbolStruct symbol) {
		final Environment currentEnvironmentInner = findClosestLambdaOrLetEnv(currentEnvironment);
		Environment bindingEnvironment = getBindingEnvironment(currentEnvironmentInner, symbol, true);

		// if there is a parent binding environment
		if (!bindingEnvironment.equals(Environment.NULL)) {
			ClosureBinding closureBinding = getClosureBinding(bindingEnvironment, symbol);

			// if there is not yet a closure for this variable, add it
			int references;
			if (closureBinding == null) {

				// there is now one reference
				references = 1;

				// get closure from the association list
				final Closure closure = bindingEnvironment.getEnvironmentClosure();

				// position will be the current size of the closure association list
				final int position = closure.getBindings().size() - 1;
				bindingEnvironment = createNewClosure(bindingEnvironment, symbol, references, position);
			} // if the binding environment has already had a closure added to it

			// for this variable, just increment it's number of references
			else {
				// get the current number of references
				references = closureBinding.getReferences();

				// increment the number of references
				references += 1;

				// set the number of references
				closureBinding.setReferences(references);
			}
		}
		return currentEnvironmentInner;
	}

	public static Environment createNewClosure(final Environment currentEnvironment, final SymbolStruct newSymbol,
											   final int references, final int position) {

		final Environment currentEnvironmentInner = findClosestLambdaOrLetEnv(currentEnvironment);

		final ClosureBinding closureBinding = new ClosureBinding(newSymbol, position, references);

		currentEnvironmentInner.getEnvironmentClosure().getBindings().add(closureBinding);
		return currentEnvironmentInner;
	}

	public static ClosureBinding getClosureBinding(final Environment currentEnvironment, final SymbolStruct variable) {
		final Environment currentEnvironmentInner = findClosestLambdaOrLetEnv(currentEnvironment);

		final Closure closure = currentEnvironmentInner.getEnvironmentClosure();

		ClosureBinding returnClosureBinding = null;
		for (final ClosureBinding closureBinding : closure.getBindings()) {
			if (closureBinding.getSymbolStruct().equals(variable)) {
				returnClosureBinding = closureBinding;
				break;
			}
		}
		return returnClosureBinding;
	}
}
