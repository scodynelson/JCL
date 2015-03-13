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

		final boolean symbolBoundInCurrentLexicalEnvironment = environment.hasLexicalBinding(input);
		if (symbolBoundInCurrentLexicalEnvironment) {
			// Binding already exists in the current lexical environment.
			return input; // TODO: fix
		}

		final BindingEnvironment bindingEnvironment
				= Environments.getLexicalBindingBindingEnvironment(environment, input);

		if (bindingEnvironment.equals(Environment.NULL)) {
			// No inner binding lexical environments. Add it as a DYNAMIC symbol in the current lexical environment before we proceed.
			analyzeDynamic(input, environment);
		}

		final LambdaEnvironment currentEnclosingLambda = Environments.getEnclosingLambda(environment);
		final LambdaEnvironment bindingEnclosingLambda = Environments.getEnclosingLambda(bindingEnvironment);

		final SymbolTable currentEnvironmentSymbolTable = environment.getSymbolTable();

		if (currentEnclosingLambda.equals(bindingEnclosingLambda)) {
			// Binding Lambda and Enclosing Lambda are the same. No need for a Closure.

			// Create a new SymbolBinding and reference it to the 'bindingEnvironment' with allocation to the 'bindingEnclosingLambda'
			final EnvironmentAllocation allocation = new EnvironmentAllocation(bindingEnclosingLambda);
			final SymbolEnvironmentBinding symbolBinding
					= new SymbolEnvironmentBinding(input, allocation, T.INSTANCE, bindingEnvironment);

			// Now add that new symbol to the SymbolTable of the 'currentEnvironment'
			currentEnvironmentSymbolTable.addLexicalEnvironmentBinding(symbolBinding);
			return input; // TODO: fix
		}

		// Here the Binding Lambda is outside of the Enclosing Lambda
		final BindingEnvironment outerBindingEnvironment
				= Environments.getLexicalBindingBindingEnvironment(environment, input);

		if (outerBindingEnvironment.equals(Environment.NULL)) {
			// Outer Binding Environment is the NULL Environment. Therefore, we can't create a Closure.

			// Create a new SymbolBinding and reference it to the 'bindingEnvironment', with allocation to the NULL Environment
			final EnvironmentAllocation allocation = new EnvironmentAllocation(Environment.NULL);
			final SymbolEnvironmentBinding symbolBinding
					= new SymbolEnvironmentBinding(input, allocation, T.INSTANCE, bindingEnvironment);

			// Now add that new symbol to the SymbolTable of the 'currentEnvironment'
			currentEnvironmentSymbolTable.addLexicalEnvironmentBinding(symbolBinding);
			return input; // TODO: fix
		}

		// There is an Outer Binding Environment. Therefore, we will create a Closure Binding in that Environment.
		final Closure closure = outerBindingEnvironment.getClosure();

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

		// Create a new SymbolBinding and reference it to the 'bindingEnvironment', with allocation to the Closure
		final ClosureAllocation allocation = new ClosureAllocation(closure);
		final SymbolClosureBinding symbolBinding
				= new SymbolClosureBinding(input, allocation, T.INSTANCE, bindingEnvironment);

		currentEnvironmentSymbolTable.addClosureBinding(symbolBinding);

		return input; // TODO: fix
	}

	@Override
	public SymbolStruct<?> analyzeDynamic(final SymbolStruct<?> input, final Environment environment) {

		final SymbolTable currentEnvironmentSymbolTable = environment.getSymbolTable();
		final boolean hasSymbolBinding = currentEnvironmentSymbolTable.hasBinding(input);

		if (hasSymbolBinding) {
			// Binding already exists in the current environment.
			return input; // TODO: fix
		}

		final LambdaEnvironment currentEnclosingLambda = Environments.getEnclosingLambda(environment);

		if (environment.equals(currentEnclosingLambda)) {
			final LambdaEnvironment currentLambda = Environments.getEnclosingLambda(environment);
			final int position = currentLambda.getNextParameterNumber();
			final LocalAllocation allocation = new LocalAllocation(position);

			final SymbolLocalBinding symbolBinding
					= new SymbolLocalBinding(input, allocation, T.INSTANCE, environment);
			currentEnvironmentSymbolTable.addDynamicLocalBinding(symbolBinding);

			return input; // TODO: fix
		}

		// Add Binding to SymbolTable in the current environment
		final EnvironmentAllocation environmentAllocation = new EnvironmentAllocation(currentEnclosingLambda);
		final SymbolEnvironmentBinding symbolBinding
				= new SymbolEnvironmentBinding(input, environmentAllocation, T.INSTANCE, currentEnclosingLambda);
		currentEnvironmentSymbolTable.addDynamicEnvironmentBinding(symbolBinding);

		final SymbolTable enclosingLambdaSymbolTable = currentEnclosingLambda.getSymbolTable();
		final boolean enclosingLambdaHasSymbolBinding = enclosingLambdaSymbolTable.hasBinding(input);

		if (enclosingLambdaHasSymbolBinding) {
			// Binding already exists in the Enclosing Lambda.
			return input; // TODO: fix
		}

		// Add Binding to SymbolTable in the Enclosing Lambda.
		final LambdaEnvironment currentLambda = Environments.getEnclosingLambda(environment);
		final int position = currentLambda.getNextParameterNumber();
		final LocalAllocation allocation = new LocalAllocation(position);

		final SymbolLocalBinding newSymbolBinding
				= new SymbolLocalBinding(input, allocation, T.INSTANCE, currentEnclosingLambda);
		enclosingLambdaSymbolTable.addDynamicLocalBinding(newSymbolBinding);

		return input; // TODO: fix
	}
}
