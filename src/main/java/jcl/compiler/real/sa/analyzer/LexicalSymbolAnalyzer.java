package jcl.compiler.real.sa.analyzer;

import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.SymbolElement;
import jcl.compiler.real.environment.BindingEnvironment;
import jcl.compiler.real.environment.Closure;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.EnvironmentStack;
import jcl.compiler.real.environment.Environments;
import jcl.compiler.real.environment.LambdaEnvironment;
import jcl.compiler.real.environment.SymbolTable;
import jcl.compiler.real.environment.allocation.ClosureAllocation;
import jcl.compiler.real.environment.allocation.EnvironmentAllocation;
import jcl.compiler.real.environment.binding.ClosureBinding;
import jcl.compiler.real.environment.binding.SymbolClosureBinding;
import jcl.compiler.real.environment.binding.SymbolEnvironmentBinding;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.types.T;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
public class LexicalSymbolAnalyzer implements Analyzer<Element, SymbolElement> {

	private static final long serialVersionUID = 231543795392423102L;

	@Autowired
	private DynamicSymbolAnalyzer dynamicSymbolAnalyzer;

	@Override
	public SymbolElement analyze(final SemanticAnalyzer analyzer, final SymbolElement input, final AnalysisBuilder analysisBuilder) {

		final EnvironmentStack environmentStack = analysisBuilder.getEnvironmentStack();
		final Environment currentEnvironment = environmentStack.peek();

		final boolean symbolBoundInCurrentLexicalEnvironment = currentEnvironment.hasLexicalBinding(input);
		if (symbolBoundInCurrentLexicalEnvironment) {
			// Binding already exists in the current lexical environment.
			return input; // TODO: fix
		}

		final BindingEnvironment bindingEnvironment
				= Environments.getLexicalBindingBindingEnvironment(currentEnvironment, input);

		if (bindingEnvironment.equals(Environment.NULL)) {
			// No inner binding lexical environments. Add it as a DYNAMIC symbol in the current lexical environment before we proceed.
			dynamicSymbolAnalyzer.analyze(analyzer, input, analysisBuilder);
		}

		final LambdaEnvironment currentEnclosingLambda = Environments.getEnclosingLambda(currentEnvironment);
		final LambdaEnvironment bindingEnclosingLambda = Environments.getEnclosingLambda(bindingEnvironment);

		final SymbolTable currentEnvironmentSymbolTable = currentEnvironment.getSymbolTable();

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
				= Environments.getLexicalBindingBindingEnvironment(currentEnvironment, input);

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
}
