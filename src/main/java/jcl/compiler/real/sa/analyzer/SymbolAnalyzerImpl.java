package jcl.compiler.real.sa.analyzer;

import java.util.Optional;

import jcl.compiler.real.environment.BindingEnvironment;
import jcl.compiler.real.environment.Closure;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.Environments;
import jcl.compiler.real.environment.LambdaEnvironment;
import jcl.compiler.real.environment.binding.ClosureBinding;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public class SymbolAnalyzerImpl implements SymbolAnalyzer {

	private static final long serialVersionUID = 4236867001501188408L;

	@Override
	public SymbolStruct<?> analyze(final SymbolStruct<?> input, final Environment environment) {

		final boolean symbolBoundInEnvironment = environment.hasLexicalBinding(input);
		if (symbolBoundInEnvironment) {
			// Binding already exists in the environment.
			return input;
		}

		final BindingEnvironment bindingEnvironment
				= Environments.getLexicalBindingBindingEnvironment(environment, input);

		final LambdaEnvironment enclosingLambda = Environments.getEnclosingLambda(environment);
		final LambdaEnvironment bindingEnclosingLambda = Environments.getEnclosingLambda(bindingEnvironment);

		if (enclosingLambda.equals(bindingEnclosingLambda)) {
			// Binding Lambda and Enclosing Lambda are the same. No need for a Closure.
			return input;
		}

		// Here the Binding Lambda is outside of the Enclosing Lambda
		final BindingEnvironment outerBindingEnvironment
				= Environments.getLexicalBindingBindingEnvironment(enclosingLambda, input);

		if (outerBindingEnvironment.equals(Environment.NULL)) {
			// Outer Binding Environment is the NULL Environment. Therefore, we can't create a Closure.
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

		return input;
	}
}
