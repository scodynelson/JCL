package jcl.compiler.real.sa;

import jcl.LispStruct;
import jcl.compiler.real.environment.Allocation;
import jcl.compiler.real.environment.Binding;
import jcl.compiler.real.environment.Closure;
import jcl.compiler.real.environment.ClosureBinding;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.EnvironmentAccessor;
import jcl.compiler.real.environment.LocalAllocation;
import jcl.compiler.real.environment.Scope;
import jcl.compiler.real.environment.SymbolBinding;
import jcl.symbols.SymbolStruct;
import jcl.types.T;
import org.springframework.stereotype.Component;

import java.util.Optional;
import java.util.Stack;

@Component
public class SymbolStructAnalyzer implements Analyzer<LispStruct, SymbolStruct<?>> {

	@Override
	public LispStruct analyze(final SemanticAnalyzer analyzer, final SymbolStruct<?> input, final AnalysisBuilder analysisBuilder) {
		final Stack<Environment> environmentStack = analysisBuilder.getEnvironmentStack();
		addSymbolToTable(environmentStack.peek(), input);
		return input;
	}

	private static Environment addSymbolToTable(final Environment currentEnvironment, final SymbolStruct<?> newSymbol) {

		// if this symbol is marked as special, it automatically goes to the second section
		if (!newSymbol.isSpecial()) {

			// This section deals with the lexical binding of symbols that are bound above the current env
			final Environment bindingEnvironment = EnvironmentAccessor.getBindingEnvironment(currentEnvironment, newSymbol, true);
			if (!bindingEnvironment.equals(Environment.NULL)) {

				// so it has a lexical binding somewhere

				final Optional<Binding> symbolBinding = currentEnvironment.getBindings()
				                                                          .stream()
				                                                          .filter(e -> e.getSymbolStruct().equals(newSymbol))
				                                                          .findFirst();

				if (!symbolBinding.isPresent()) {

					// ...(:BINDING #the-binding-env# :SCOPE :LEXICAL :TYPE T)
					// so we just have to point the allocation at the binding environment
					//*** here is where we know that there is a closure involved ***
					// See if it is bound in the current lambda env. If so, just carry on
					final Environment currentLambda = EnvironmentAccessor.getEnclosingLambda(currentEnvironment);

					// is the binding location above the current lambda? If so, there has
					// to be a closure allocation in the bindingEnvironment
					final Environment bindingLambda = EnvironmentAccessor.getEnclosingLambda(bindingEnvironment);
					if (!currentLambda.equals(bindingLambda)) {

						// here the binding lambda is outside the enclosing lambda
						// this calls for a closure allocation in the outer env (lambda or let)
						addClosureToBindingEnvironment(bindingEnvironment, newSymbol);
					}

					final SymbolBinding newSymbolBinding = new SymbolBinding(newSymbol, null, Scope.LEXICAL, T.INSTANCE, bindingEnvironment);
					currentEnvironment.getSymbolTable().getBindings().add(newSymbolBinding);
				}

				// we may also be here because there is a binding right here. Nothing to do
				return currentEnvironment;
			}
		}


		// if not bound anywhere in the binding tree (free and dynamic)...
		// we know we're dealing with dynamic variables - so, is it already tagged in this environment?
		if (EnvironmentAccessor.getSymbolInTable(currentEnvironment, newSymbol) != null) {
			// yes, nothing to do
			return currentEnvironment;
		}

		// so, we at least have to add an entry in this environment
		// and possibly in an outer one

		// ...(:BINDING :FREE :SCOPE :DYNAMIC :TYPE T)

		final Allocation finalAllocation;
		final Environment finalBindingEnvironment;
		// if the current environment is not a lambda, we may need to add an entry to the enclosing lambda
		if (EnvironmentAccessor.isLambda(currentEnvironment)) {

			// symbol was found in the immediate scope of the Lambda
			finalAllocation = addLocalAlloc(currentEnvironment);
			finalBindingEnvironment = Environment.FREE;

			// ...(:ALLOCATION (:LOCAL . n) :BINDING :FREE :SCOPE :DYNAMIC :TYPE T)

			// now we add the new symbol to the local table
			final SymbolBinding symbolBinding = new SymbolBinding(newSymbol, finalAllocation, Scope.DYNAMIC, T.INSTANCE, finalBindingEnvironment);
			currentEnvironment.getSymbolTable().getBindings().add(symbolBinding);
			return currentEnvironment;
		}

		// if the current environment is not a lambda, find the enclosing lambda
		final Environment enclosingLambda = EnvironmentAccessor.getEnclosingLambda(currentEnvironment);

		// see if the symbol is already in the enclosing environment
		if (EnvironmentAccessor.getSymbolInTable(enclosingLambda, newSymbol) == null) {

			// it's not in the enclosing lambda, so put in an entry
			final Allocation enclosingSymTbl = addLocalAlloc(currentEnvironment);

			// add to the enclosing lambda's symbol table
			final SymbolBinding symbolBinding = new SymbolBinding(newSymbol, enclosingSymTbl, Scope.DYNAMIC, T.INSTANCE, Environment.FREE);
			enclosingLambda.getSymbolTable().getBindings().add(symbolBinding);

			// Now we have the outer symbol table set up correctly
			// set the Allocation KeywordOld to what is should be for the current environment

			// ...(:ALLOCATION #enclosing-lambda# :BINDING :FREE :SCOPE :DYNAMIC :TYPE T)

			// now we add the new symbol to the local table
			final SymbolBinding finalSymbolBinding = new SymbolBinding(newSymbol, null, Scope.DYNAMIC, T.INSTANCE, enclosingLambda);
			currentEnvironment.getSymbolTable().getBindings().add(finalSymbolBinding);
			return currentEnvironment;
		}

		// it is, so we have to add a reference to that environment in the current env

		final SymbolBinding symbolBinding = new SymbolBinding(newSymbol, null, Scope.DYNAMIC, T.INSTANCE, enclosingLambda);
		currentEnvironment.getSymbolTable().getBindings().add(symbolBinding);
		return currentEnvironment;
	}

	private static Allocation addLocalAlloc(final Environment currentEnvironment) {
		final int localNumber = EnvironmentAccessor.getNextAvailableParameterNumber(currentEnvironment);
		return new LocalAllocation(localNumber);
	}

	private static Environment addClosureToBindingEnvironment(final Environment currentEnvironment, final SymbolStruct<?> symbol) {

		Environment bindingEnvironment = EnvironmentAccessor.getBindingEnvironment(currentEnvironment, symbol, true);

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
		return currentEnvironment;
	}

	private static Environment createNewClosure(final Environment currentEnvironment, final SymbolStruct<?> newSymbol,
	                                           final int references, final int position) {

		final ClosureBinding closureBinding = new ClosureBinding(newSymbol, position, references);

		currentEnvironment.getEnvironmentClosure().getBindings().add(closureBinding);
		return currentEnvironment;
	}

	private static ClosureBinding getClosureBinding(final Environment currentEnvironment, final SymbolStruct<?> variable) {

		final Closure closure = currentEnvironment.getEnvironmentClosure();

		return closure.getBindings()
		              .stream()
		              .filter(e -> e.getSymbolStruct().equals(variable))
		              .findFirst()
		              .orElse(null);
	}

}
