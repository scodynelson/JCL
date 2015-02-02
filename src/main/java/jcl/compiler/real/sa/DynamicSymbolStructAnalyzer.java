package jcl.compiler.real.sa;

import jcl.compiler.real.environment.Allocation;
import jcl.compiler.real.environment.DynamicEnvironment;
import jcl.compiler.real.environment.EnvironmentAccessor;
import jcl.compiler.real.environment.EnvironmentAllocation;
import jcl.compiler.real.environment.LexicalEnvironment;
import jcl.compiler.real.environment.LocalAllocation;
import jcl.compiler.real.environment.Scope;
import jcl.compiler.real.environment.SymbolBinding;
import jcl.compiler.real.environment.SymbolTable;
import jcl.compiler.real.sa.element.SymbolElement;
import jcl.symbols.SymbolStruct;
import jcl.types.T;
import org.springframework.stereotype.Component;

import java.util.Optional;
import java.util.Stack;

@Component
public class DynamicSymbolStructAnalyzer extends SymbolStructAnalyzer {

	@Override
	public SymbolElement<?> analyzeSymbol(final SymbolStruct<?> input, final AnalysisBuilder analysisBuilder) {
		final Stack<LexicalEnvironment> lexicalEnvironmentStack = analysisBuilder.getLexicalEnvironmentStack();

		final LexicalEnvironment lexicalEnvironment = lexicalEnvironmentStack.peek();

		final SymbolTable symbolTable = lexicalEnvironment.getSymbolTable();
		final Optional<SymbolBinding> bindingInSymbolTable = symbolTable.getBinding(input);

		if (bindingInSymbolTable.isPresent()) {
			// Binding already exists in the current lexical environment.
			return new SymbolElement<>(input);
		}

		final LexicalEnvironment currentEnclosingLambda = getEnclosingLambda(lexicalEnvironment);

		final Stack<DynamicEnvironment> dynamicEnvironmentStack = analysisBuilder.getDynamicEnvironmentStack();
		final DynamicEnvironment dynamicEnvironment = dynamicEnvironmentStack.peek();

		if (lexicalEnvironment.equals(currentEnclosingLambda)) {
			final int position = EnvironmentAccessor.getNextAvailableParameterNumber(lexicalEnvironment);
			final Allocation allocation = new LocalAllocation(position);

			final SymbolBinding symbolBinding = new SymbolBinding(input, allocation, Scope.DYNAMIC, T.INSTANCE, dynamicEnvironment);
			symbolTable.addBinding(symbolBinding);

			return new SymbolElement<>(input);
		}

		// Add Binding to SymbolTable in the current lexical environment
		final EnvironmentAllocation environmentAllocation = new EnvironmentAllocation(currentEnclosingLambda);
		final SymbolBinding symbolBinding = new SymbolBinding(input, environmentAllocation, Scope.DYNAMIC, T.INSTANCE, currentEnclosingLambda);
		symbolTable.addBinding(symbolBinding);

		final SymbolTable enclosingLambdaSymbolTable = currentEnclosingLambda.getSymbolTable();
		final Optional<SymbolBinding> enclosingSymbolBinding = enclosingLambdaSymbolTable.getBinding(input);

		if (enclosingSymbolBinding.isPresent()) {
			// Binding already exists in the Enclosing Lambda.
			return new SymbolElement<>(input);
		}

		// Add Binding to SymbolTable in the Enclosing Lambda.
		final int position = EnvironmentAccessor.getNextAvailableParameterNumber(lexicalEnvironment);
		final Allocation allocation = new LocalAllocation(position);

		final SymbolBinding newSymbolBinding = new SymbolBinding(input, allocation, Scope.DYNAMIC, T.INSTANCE, dynamicEnvironment);
		enclosingLambdaSymbolTable.addBinding(newSymbolBinding);

		return new SymbolElement<>(input);
	}
}
