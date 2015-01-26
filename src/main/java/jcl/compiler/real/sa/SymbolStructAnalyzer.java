package jcl.compiler.real.sa;

import jcl.LispStruct;
import jcl.compiler.real.environment.Allocation;
import jcl.compiler.real.environment.Binding;
import jcl.compiler.real.environment.Closure;
import jcl.compiler.real.environment.ClosureBinding;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.EnvironmentAccessor;
import jcl.compiler.real.environment.EnvironmentAllocation;
import jcl.compiler.real.environment.LocalAllocation;
import jcl.compiler.real.environment.Scope;
import jcl.compiler.real.environment.SymbolBinding;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Optional;
import java.util.Stack;

@Component
public class SymbolStructAnalyzer implements Analyzer<LispStruct, SymbolStruct<?>> {

	@Override
	public LispStruct analyze(final SemanticAnalyzer analyzer, final SymbolStruct<?> input, final AnalysisBuilder analysisBuilder) {
		final Stack<Environment> environmentStack = analysisBuilder.getEnvironmentStack();

		final Environment environment = environmentStack.peek();
		if (input.isSpecial()) {
			addDynamicSymbolToEnvironment(environmentStack.peek(), input);
		} else {
			addLexicalSymbolToEnvironment(environment, input);
		}

		return input;
	}

	public LispStruct analyze(final SymbolStruct<?> input, final AnalysisBuilder analysisBuilder, final boolean isSpecial) {
		final Stack<Environment> environmentStack = analysisBuilder.getEnvironmentStack();

		final Environment environment = environmentStack.peek();
		if (isSpecial) {
			addDynamicSymbolToEnvironment(environment, input);
		} else {
			addLexicalSymbolToEnvironment(environment, input);
		}

		return input;
	}

	private static void addDynamicSymbolToEnvironment(final Environment environment, final SymbolStruct<?> newSymbol) {

		// if not bound anywhere in the binding tree (free and dynamic)...
		// we know we're dealing with dynamic variables - so, is it already tagged in this environment?
		if (EnvironmentAccessor.getSymbolInTable(environment, newSymbol) != null) {
			// yes, nothing to do
			return;
		}

		// so, we at least have to add an entry in this environment
		// and possibly in an outer one

		// ...(:BINDING :FREE :SCOPE :DYNAMIC :TYPE T)

		// if the current environment is not a lambda, we may need to add an entry to the enclosing lambda
		if (EnvironmentAccessor.isLambda(environment)) {

			// symbol was found in the immediate scope of the Lambda
			final Allocation finalAllocation = addLocalAlloc(environment);

			// ...(:ALLOCATION (:LOCAL . n) :BINDING :FREE :SCOPE :DYNAMIC :TYPE T)

			// now we add the new symbol to the local table
			environment.getSymbolTable().addBinding(newSymbol, finalAllocation, Scope.DYNAMIC, Environment.FREE);
			return;
		}

		// if the current environment is not a lambda, find the enclosing lambda
		final Environment enclosingLambda = EnvironmentAccessor.getEnclosingLambda(environment);

		// see if the symbol is already in the enclosing environment
		final SymbolBinding enclosingSymbolBinding = EnvironmentAccessor.getSymbolInTable(enclosingLambda, newSymbol);
		if (enclosingSymbolBinding == null) {
			// it is, so we have to add a reference to that environment in the current env

			// it's not in the enclosing lambda, so put in an entry
			final Allocation enclosingSymTbl = addLocalAlloc(environment);

			// add to the enclosing lambda's symbol table
			enclosingLambda.getSymbolTable().addBinding(newSymbol, enclosingSymTbl, Scope.DYNAMIC, Environment.FREE);
		}

		// Now we have the outer symbol table set up correctly
		// set the Allocation KeywordOld to what is should be for the current environment

		// ...(:ALLOCATION #enclosing-lambda# :BINDING :FREE :SCOPE :DYNAMIC :TYPE T)
		environment.getSymbolTable().addBinding(newSymbol, new EnvironmentAllocation(enclosingLambda), Scope.DYNAMIC, enclosingLambda);
	}

	private static Allocation addLocalAlloc(final Environment currentEnvironment) {
		final int localNumber = EnvironmentAccessor.getNextAvailableParameterNumber(currentEnvironment);
		return new LocalAllocation(localNumber);
	}

	private static void addLexicalSymbolToEnvironment(final Environment currentEnvironment, final SymbolStruct<?> newSymbol) {
		final Environment bindingEnvironment = EnvironmentAccessor.getBindingEnvironment(currentEnvironment, newSymbol, true);

		if (bindingEnvironment.equals(Environment.NULL)) {
			addDynamicSymbolToEnvironment(currentEnvironment, newSymbol);
		}

		final List<Binding> environmentBindings = currentEnvironment.getBindings();

		final Optional<Binding> currentSymbolBinding
				= environmentBindings.stream()
				                     .filter(e -> e.getSymbolStruct().equals(newSymbol))
				                     .findFirst();

		// Does it have a lexical binding the in current environment?
		if (currentSymbolBinding.isPresent()) {
			return;
		}

		// ...(:BINDING #the-binding-env# :SCOPE :LEXICAL :TYPE T)
		// so we just have to point the allocation at the binding environment
		//*** here is where we know that there is a closure involved ***
		// See if it is bound in the current lambda env. If so, just carry on

		final Environment currentLambda = EnvironmentAccessor.getEnclosingLambda(currentEnvironment);
		final Environment bindingLambda = EnvironmentAccessor.getEnclosingLambda(bindingEnvironment);

		// is the binding location above the current lambda? If so, there has
		// to be a closure allocation in the bindingEnvironment
		if (currentLambda.equals(bindingLambda)) {
			// Create a new SymbolBinding and reference it to the 'bindingEnvironment'
			// Now add that new symbol to the SymbolTable of the 'currentEnvironment'
			currentEnvironment.getSymbolTable().addBinding(newSymbol, new EnvironmentAllocation(bindingLambda), Scope.LEXICAL, bindingEnvironment);
			return;
		}

		// here the binding lambda is outside the enclosing lambda
		// this calls for a closure allocation in the outer env (lambda or let)
		final Environment outerBindingEnvironment = EnvironmentAccessor.getBindingEnvironment(bindingEnvironment, newSymbol, true);

		// if there is a parent binding environment
		if (outerBindingEnvironment.equals(Environment.NULL)) {
			// Create a new SymbolBinding and reference it to the 'bindingEnvironment'
			// Now add that new symbol to the SymbolTable of the 'currentEnvironment'
			currentEnvironment.getSymbolTable().addBinding(newSymbol, new EnvironmentAllocation(Environment.NULL), Scope.LEXICAL, bindingEnvironment);
			return;
		}

		final Closure closure = outerBindingEnvironment.getEnvironmentClosure();
		final List<ClosureBinding> closureBindings = closure.getBindings();

		final Optional<ClosureBinding> closureBinding
				= closureBindings.stream()
				                 .filter(e -> e.getSymbolStruct().equals(newSymbol))
				                 .findFirst();

		// if there is not yet a closure for this variable, add it
		if (closureBinding.isPresent()) {
			final ClosureBinding closureBindingValue = closureBinding.get();
			// if the binding environment has already had a closure added to it
			// for this variable, just increment it's number of references

			// increment the number of references
			closureBindingValue.incrementReferences();
		} else {
			closure.addBinding(newSymbol);
		}
	}

}
