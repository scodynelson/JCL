package jcl.compiler.real.sa.analyzer.specialoperator;

import jcl.compiler.real.element.ConsElement;
import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.SimpleElement;
import jcl.compiler.real.element.SymbolElement;
import jcl.compiler.real.element.specialoperator.ImmutableLoadTimeValueElement;
import jcl.compiler.real.element.specialoperator.LoadTimeValueElement;
import jcl.compiler.real.element.specialoperator.MutableLoadTimeValueElement;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.EnvironmentStack;
import jcl.compiler.real.environment.Environments;
import jcl.compiler.real.environment.LoadTimeValue;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.analyzer.expander.real.MacroFunctionExpander;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.BooleanStruct;
import jcl.symbols.SpecialOperator;
import jcl.system.EnhancedLinkedList;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import java.util.List;
import java.util.UUID;

@Component
public class LoadTimeValueAnalyzer extends MacroFunctionExpander implements SpecialOperatorAnalyzer {

	private static final long serialVersionUID = 2168018740373766746L;

	/**
	 * Initializes the block macro function and adds it to the special operator 'block'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperator.LOAD_TIME_VALUE.setMacroFunctionExpander(this);
	}

	@Override
	public Element expand(final ConsElement form, final AnalysisBuilder analysisBuilder) {
		return analyze(form, analysisBuilder);
	}

	@Override
	public LoadTimeValueElement analyze(final ConsElement input, final AnalysisBuilder analysisBuilder) {

		final EnhancedLinkedList<SimpleElement> elements = input.getElements();

		final int inputSize = elements.size();
		if ((inputSize < 2) || (inputSize > 3)) {
			throw new ProgramErrorException("LOAD-TIME-VALUE: Incorrect number of arguments: " + inputSize + ". Expected either 2 or 3 arguments.");
		}

		final EnhancedLinkedList<SimpleElement> inputRest = elements.getAllButFirst();
		final EnhancedLinkedList<SimpleElement> inputRestRest = inputRest.getAllButFirst();

		final SimpleElement third = inputRestRest.getFirst();
		if (!(third instanceof BooleanStruct)) { // TODO: fix
			throw new ProgramErrorException("LOAD-TIME-VALUE: Read-Only-P value must be of type BooleanStruct. Got: " + third);
		}

		final BooleanStruct readOnlyP = (BooleanStruct) third;
		final boolean isReadOnly = readOnlyP.booleanValue();

		final SimpleElement form = inputRest.getFirst();

		final SymbolElement evalFnSym = new SymbolElement(GlobalPackageStruct.COMMON_LISP.getName(), "EVAL");
		final ConsElement evalForm = new ConsElement(evalFnSym, form);

		final EnvironmentStack environmentStack = analysisBuilder.getEnvironmentStack();
		final Environment currentEnvironment = environmentStack.peek();
		final Environment currentEnclosingLambda = Environments.getEnclosingLambda(currentEnvironment);

		final int tempClosureDepth = analysisBuilder.getClosureDepth();
		final int newClosureDepth = tempClosureDepth + 1;

		final Environment nullLexicalEnvironment = Environment.NULL;
		environmentStack.push(nullLexicalEnvironment);

		final int tempBindingsPosition = analysisBuilder.getBindingsPosition();
		try {
			analysisBuilder.setClosureDepth(newClosureDepth);

			final SemanticAnalyzer analyzer = analysisBuilder.getAnalyzer();

			final Element analyzedEvalForm = analyzer.analyzeForm(evalForm, analysisBuilder);

			if (isReadOnly) {
				final UUID uniqueLTVId = UUID.randomUUID();

				// TODO: move LTVs to LambdaEnvironment???
				final List<LoadTimeValue> currentLoadTimeValues = currentEnclosingLambda.getLoadTimeValues();

				final LoadTimeValue newLoadTimeValue = new LoadTimeValue(uniqueLTVId, analyzedEvalForm);
				currentLoadTimeValues.add(newLoadTimeValue);

				return new ImmutableLoadTimeValueElement(uniqueLTVId);
			} else {
				return new MutableLoadTimeValueElement(analyzedEvalForm);
			}
		} finally {
			analysisBuilder.setClosureDepth(tempClosureDepth);
			analysisBuilder.setBindingsPosition(tempBindingsPosition);
			environmentStack.pop();
		}
	}
}
