package jcl.compiler.real.sa.analyzer;

import jcl.compiler.real.element.SymbolElement;
import jcl.compiler.real.environment.BindingEnvironment;
import jcl.compiler.real.environment.Closure;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.EnvironmentStack;
import jcl.compiler.real.environment.Environments;
import jcl.compiler.real.environment.LambdaEnvironment;
import jcl.compiler.real.environment.Scope;
import jcl.compiler.real.environment.SymbolTable;
import jcl.compiler.real.environment.allocation.EnvironmentAllocation;
import jcl.compiler.real.environment.binding.ClosureBinding;
import jcl.compiler.real.environment.binding.SymbolEnvironmentBinding;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.symbols.SymbolStruct;
import jcl.types.T;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
public class LexicalSymbolStructAnalyzer extends SymbolStructAnalyzer {

	private static final long serialVersionUID = 231543795392423102L;

	@Autowired
	private DynamicSymbolStructAnalyzer dynamicSymbolStructAnalyzer;

	@Override
	public SymbolElement<?> analyze(final SemanticAnalyzer analyzer, final SymbolStruct<?> input, final AnalysisBuilder analysisBuilder) {

		final EnvironmentStack environmentStack = analysisBuilder.getEnvironmentStack();
		final Environment currentEnvironment = environmentStack.peek();

		final boolean symbolBoundInCurrentLexicalEnvironment = currentEnvironment.hasLexicalBinding(input);
		if (symbolBoundInCurrentLexicalEnvironment) {
			// Binding already exists in the current lexical environment.
			return new SymbolElement<>(input);
		}

		final BindingEnvironment bindingEnvironment
				= Environments.getLexicalBindingBindingEnvironment(currentEnvironment, input);

		if (bindingEnvironment.equals(Environment.NULL)) {
			// No inner binding lexical environments. Add it as a DYNAMIC symbol in the current lexical environment before we proceed.
			dynamicSymbolStructAnalyzer.analyze(analyzer, input, analysisBuilder);
		}

		final LambdaEnvironment currentEnclosingLambda = Environments.getEnclosingLambda(currentEnvironment);
		final LambdaEnvironment bindingEnclosingLambda = Environments.getEnclosingLambda(bindingEnvironment);

		final SymbolTable currentEnvironmentSymbolTable = currentEnvironment.getSymbolTable();

		if (currentEnclosingLambda.equals(bindingEnclosingLambda)) {
			// Binding Lambda and Enclosing Lambda are the same. No need for a Closure.

			// Create a new SymbolBinding and reference it to the 'bindingEnvironment' with allocation to the 'bindingEnclosingLambda'
			final EnvironmentAllocation allocation = new EnvironmentAllocation(bindingEnclosingLambda);
			// TODO: get rid of Scope
			final SymbolEnvironmentBinding symbolBinding
					= new SymbolEnvironmentBinding(input, allocation, Scope.LEXICAL, T.INSTANCE, bindingEnvironment);

			// Now add that new symbol to the SymbolTable of the 'currentEnvironment'
			currentEnvironmentSymbolTable.addEnvironmentBinding(symbolBinding);
			return new SymbolElement<>(input);
		}

		// Here the Binding Lambda is outside of the Enclosing Lambda
		final BindingEnvironment outerBindingEnvironment
				= Environments.getLexicalBindingBindingEnvironment(currentEnvironment, input);

		if (outerBindingEnvironment.equals(Environment.NULL)) {
			// Outer Binding Environment is the NULL Environment. Therefore, we can't create a Closure.

			// Create a new SymbolBinding and reference it to the 'bindingEnvironment', with allocation to the NULL Environment
			final EnvironmentAllocation allocation = new EnvironmentAllocation(Environment.NULL);
			// TODO: get rid of Scope
			final SymbolEnvironmentBinding symbolBinding
					= new SymbolEnvironmentBinding(input, allocation, Scope.LEXICAL, T.INSTANCE, bindingEnvironment);

			// Now add that new symbol to the SymbolTable of the 'currentEnvironment'
			currentEnvironmentSymbolTable.addEnvironmentBinding(symbolBinding);
			return new SymbolElement<>(input);
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

		// TODO: we need to add this binding to the environment AFTER we add it to the closure??

		return new SymbolElement<>(input);
	}
}
