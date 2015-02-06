package jcl.compiler.real.sa.analyzer;

import jcl.compiler.real.environment.DynamicEnvironment;
import jcl.compiler.real.environment.EnvironmentAccessor;
import jcl.compiler.real.environment.allocation.EnvironmentAllocation;
import jcl.compiler.real.environment.LexicalEnvironment;
import jcl.compiler.real.environment.allocation.LocalAllocation;
import jcl.compiler.real.environment.Scope;
import jcl.compiler.real.environment.binding.SymbolEnvironmentBinding;
import jcl.compiler.real.environment.binding.SymbolLocalBinding;
import jcl.compiler.real.environment.SymbolTable;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.element.SymbolElement;
import jcl.symbols.SymbolStruct;
import jcl.types.T;
import org.springframework.stereotype.Component;

import java.util.Stack;

@Component
public class DynamicSymbolStructAnalyzer extends SymbolStructAnalyzer {

	private static final long serialVersionUID = 4236867001501188408L;

	@Override
	public SymbolElement<?> analyze(final SemanticAnalyzer analyzer, final SymbolStruct<?> input, final AnalysisBuilder analysisBuilder) {

		final Stack<LexicalEnvironment> lexicalEnvironmentStack = analysisBuilder.getLexicalEnvironmentStack();
		final LexicalEnvironment currentLexicalEnvironment = lexicalEnvironmentStack.peek();

		final SymbolTable currentLexicalEnvironmentSymbolTable = currentLexicalEnvironment.getSymbolTable();
		final boolean hasSymbolBinding = currentLexicalEnvironmentSymbolTable.hasBinding(input);

		if (hasSymbolBinding) {
			// Binding already exists in the current lexical environment.
			return new SymbolElement<>(input);
		}

		final LexicalEnvironment currentEnclosingLambda = getEnclosingLambda(currentLexicalEnvironment);

		final Stack<DynamicEnvironment> dynamicEnvironmentStack = analysisBuilder.getDynamicEnvironmentStack();
		final DynamicEnvironment dynamicEnvironment = dynamicEnvironmentStack.peek();

		if (currentLexicalEnvironment.equals(currentEnclosingLambda)) {
			final int position = EnvironmentAccessor.getNextAvailableParameterNumber(currentLexicalEnvironment);
			final LocalAllocation allocation = new LocalAllocation(position);

			final SymbolLocalBinding symbolBinding
					= new SymbolLocalBinding(input, allocation, Scope.DYNAMIC, T.INSTANCE, dynamicEnvironment);
			currentLexicalEnvironmentSymbolTable.addLocalBinding(symbolBinding);

			return new SymbolElement<>(input);
		}

		// Add Binding to SymbolTable in the current lexical environment
		final EnvironmentAllocation environmentAllocation = new EnvironmentAllocation(currentEnclosingLambda);
		final SymbolEnvironmentBinding symbolBinding
				= new SymbolEnvironmentBinding(input, environmentAllocation, Scope.DYNAMIC, T.INSTANCE, currentEnclosingLambda);
		currentLexicalEnvironmentSymbolTable.addEnvironmentBinding(symbolBinding);

		final SymbolTable enclosingLambdaSymbolTable = currentEnclosingLambda.getSymbolTable();
		final boolean enclosingLambdaHasSymbolBinding = enclosingLambdaSymbolTable.hasBinding(input);

		if (enclosingLambdaHasSymbolBinding) {
			// Binding already exists in the Enclosing Lambda.
			return new SymbolElement<>(input);
		}

		// Add Binding to SymbolTable in the Enclosing Lambda.
		final int position = EnvironmentAccessor.getNextAvailableParameterNumber(currentLexicalEnvironment);
		final LocalAllocation allocation = new LocalAllocation(position);

		final SymbolLocalBinding newSymbolBinding
				= new SymbolLocalBinding(input, allocation, Scope.DYNAMIC, T.INSTANCE, dynamicEnvironment);
		enclosingLambdaSymbolTable.addLocalBinding(newSymbolBinding);

		return new SymbolElement<>(input);
	}
}
