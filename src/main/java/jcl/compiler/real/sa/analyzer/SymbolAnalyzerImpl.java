package jcl.compiler.real.sa.analyzer;

import java.util.Optional;

import jcl.compiler.real.environment.BindingEnvironment;
import jcl.compiler.real.environment.Closure;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.Environments;
import jcl.compiler.real.environment.LambdaEnvironment;
import jcl.compiler.real.environment.SymbolTable;
import jcl.compiler.real.environment.allocation.ClosureAllocation;
import jcl.compiler.real.environment.allocation.EnvironmentAllocation;
import jcl.compiler.real.environment.allocation.LocalAllocation;
import jcl.compiler.real.environment.binding.ClosureBinding;
import jcl.compiler.real.environment.binding.SymbolClosureBinding;
import jcl.compiler.real.environment.binding.SymbolEnvironmentBinding;
import jcl.compiler.real.environment.binding.SymbolLocalBinding;
import jcl.symbols.SymbolStruct;
import jcl.types.T;
import org.springframework.stereotype.Component;

@Component
public class SymbolAnalyzerImpl implements SymbolAnalyzer {

	private static final long serialVersionUID = 4236867001501188408L;

	@Override
	public SymbolStruct<?> analyze(final SymbolStruct<?> input, final Environment environment) {
		return analyzeLexical(input, environment);
	}

	@Override
	public SymbolStruct<?> analyzeLexical(final SymbolStruct<?> input, final Environment environment) {

		final boolean symbolBoundInEnvironment = environment.hasLexicalBinding(input);
		if (symbolBoundInEnvironment) {
			// Binding already exists in the environment.
			return input;
		}

		final BindingEnvironment bindingEnvironment
				= Environments.getLexicalBindingBindingEnvironment(environment, input);

		if (bindingEnvironment.equals(Environment.NULL)) {
			// No inner binding environment. Add it as a DYNAMIC symbol in the environment before we proceed.
			analyzeDynamic(input, environment);
		}

		final LambdaEnvironment enclosingLambda = Environments.getEnclosingLambda(environment);
		final LambdaEnvironment bindingEnclosingLambda = Environments.getEnclosingLambda(bindingEnvironment);

		final SymbolTable symbolTable = environment.getSymbolTable();

		if (enclosingLambda.equals(bindingEnclosingLambda)) {
			// Binding Lambda and Enclosing Lambda are the same. No need for a Closure.

			// Create a new SymbolBinding and reference it to the 'bindingEnvironment' with allocation to the 'bindingEnclosingLambda'
			final EnvironmentAllocation allocation = new EnvironmentAllocation(bindingEnclosingLambda);
			final SymbolEnvironmentBinding symbolBinding
					= new SymbolEnvironmentBinding(input, allocation, T.INSTANCE, bindingEnvironment);

			// Now add that new symbol to the SymbolTable of the 'environment'
			symbolTable.addLexicalEnvironmentBinding(symbolBinding);
			return input;
		}

		// Here the Binding Lambda is outside of the Enclosing Lambda
		final BindingEnvironment outerBindingEnvironment
				= Environments.getLexicalBindingBindingEnvironment(enclosingLambda, input);

		if (outerBindingEnvironment.equals(Environment.NULL)) {
			// Outer Binding Environment is the NULL Environment. Therefore, we can't create a Closure.

			// Create a new SymbolBinding and reference it to the 'bindingEnvironment', with allocation to the NULL Environment
			final EnvironmentAllocation allocation = new EnvironmentAllocation(Environment.NULL);
			final SymbolEnvironmentBinding symbolBinding
					= new SymbolEnvironmentBinding(input, allocation, T.INSTANCE, bindingEnvironment);

			// Now add that new symbol to the SymbolTable of the 'currentEnvironment'
			symbolTable.addLexicalEnvironmentBinding(symbolBinding);
			return input;
		}

		// There is an Outer Binding Environment. Therefore, we will create a Closure Binding in that Environment.
		final Closure closure = enclosingLambda.getClosure();

		final Optional<ClosureBinding> closureBinding = closure.getBinding(input);

		if (closureBinding.isPresent()) {
			// Closure Binding already exists in the Outer Binding Environment.

			final ClosureBinding closureBindingValue = closureBinding.get();

			// Increment the number of references to this Closure Binding.
			closureBindingValue.incrementReferences();
		} else {
			// Create a new ClosureBinding in the Outer Binding Environment.
			final ClosureBinding newClosureBinding = new ClosureBinding(input, closure.getBindings().size(), 1);
			closure.addBinding(newClosureBinding);
		}

		// Create a new SymbolBinding and reference it to the 'outerBindingEnvironment', with allocation to the Closure
		final ClosureAllocation allocation = new ClosureAllocation(closure);
		final SymbolClosureBinding symbolBinding
				= new SymbolClosureBinding(input, allocation, T.INSTANCE, outerBindingEnvironment);

		symbolTable.addClosureBinding(symbolBinding);

		return input;
	}

	@Override
	public SymbolStruct<?> analyzeDynamic(final SymbolStruct<?> input, final Environment environment) {

		final SymbolTable symbolTable = environment.getSymbolTable();
		final boolean hasSymbolBinding = symbolTable.hasBinding(input);

		if (hasSymbolBinding) {
			// Binding already exists in the environment.
			return input;
		}

		final LambdaEnvironment enclosingLambda = Environments.getEnclosingLambda(environment);
		final SymbolTable enclosingLambdaSymbolTable = enclosingLambda.getSymbolTable();

		if (environment.equals(enclosingLambda)) {
			final int position = enclosingLambda.getNextParameterNumber();
			final LocalAllocation allocation = new LocalAllocation(position);

			final SymbolLocalBinding symbolBinding
					= new SymbolLocalBinding(input, allocation, T.INSTANCE, enclosingLambda);
			enclosingLambdaSymbolTable.addDynamicLocalBinding(symbolBinding);

			return input;
		}

		// Add Binding to SymbolTable in the environment
		final EnvironmentAllocation environmentAllocation = new EnvironmentAllocation(enclosingLambda);
		final SymbolEnvironmentBinding environmentSymbolBinding
				= new SymbolEnvironmentBinding(input, environmentAllocation, T.INSTANCE, enclosingLambda);
		symbolTable.addDynamicEnvironmentBinding(environmentSymbolBinding);

		final boolean enclosingLambdaHasSymbolBinding = enclosingLambdaSymbolTable.hasBinding(input);

		if (enclosingLambdaHasSymbolBinding) {
			// Binding already exists in the Enclosing Lambda.
			return input;
		}

		// Add Binding to SymbolTable in the Enclosing Lambda.
		final int position = enclosingLambda.getNextParameterNumber();
		final LocalAllocation allocation = new LocalAllocation(position);

		final SymbolLocalBinding symbolBinding
				= new SymbolLocalBinding(input, allocation, T.INSTANCE, enclosingLambda);
		enclosingLambdaSymbolTable.addDynamicLocalBinding(symbolBinding);

		return input;
	}
}
