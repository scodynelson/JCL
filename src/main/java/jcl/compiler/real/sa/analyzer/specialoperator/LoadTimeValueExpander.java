package jcl.compiler.real.sa.analyzer.specialoperator;

import java.util.List;
import java.util.UUID;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.AnalysisBuilder;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.EnvironmentStack;
import jcl.compiler.real.environment.Environments;
import jcl.compiler.real.environment.LoadTimeValue;
import jcl.compiler.real.sa.FormAnalyzer;
import jcl.compiler.real.sa.analyzer.expander.real.MacroFunctionExpander;
import jcl.compiler.real.struct.specialoperator.ImmutableLoadTimeValueStruct;
import jcl.compiler.real.struct.specialoperator.LoadTimeValueStruct;
import jcl.compiler.real.struct.specialoperator.MutableLoadTimeValueStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.symbols.BooleanStruct;
import jcl.symbols.SpecialOperator;
import jcl.system.CommonLispSymbols;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class LoadTimeValueExpander extends MacroFunctionExpander<LoadTimeValueStruct> {

	private static final long serialVersionUID = 2168018740373766746L;

	@Autowired
	private FormAnalyzer formAnalyzer;

	/**
	 * Initializes the block macro function and adds it to the special operator 'block'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperator.LOAD_TIME_VALUE.setMacroFunctionExpander(this);
	}

	@Override
	public LoadTimeValueStruct expand(final ListStruct form, final Environment environment) {

		final int inputSize = form.size();
		if ((inputSize < 2) || (inputSize > 3)) {
			throw new ProgramErrorException("LOAD-TIME-VALUE: Incorrect number of arguments: " + inputSize + ". Expected either 2 or 3 arguments.");
		}

		final ListStruct inputRest = form.getRest();
		final ListStruct inputRestRest = inputRest.getRest();

		final LispStruct third = inputRestRest.getFirst();
		if (!(third instanceof BooleanStruct)) {
			throw new ProgramErrorException("LOAD-TIME-VALUE: Read-Only-P value must be of type BooleanStruct. Got: " + third);
		}

		final BooleanStruct readOnlyP = (BooleanStruct) third;
		final boolean isReadOnly = readOnlyP.booleanValue();

		final LispStruct ltvForm = inputRest.getFirst();
		final ListStruct evalForm = ListStruct.buildProperList(CommonLispSymbols.EVAL, ltvForm);

		final AnalysisBuilder analysisBuilder = environment.getAnalysisBuilder();
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

			final LispStruct analyzedEvalForm = formAnalyzer.analyze(evalForm, nullLexicalEnvironment);

			if (isReadOnly) {
				final UUID uniqueLTVId = UUID.randomUUID();

				// TODO: move LTVs to LambdaEnvironment???
				final List<LoadTimeValue> currentLoadTimeValues = currentEnclosingLambda.getLoadTimeValues();

				final LoadTimeValue newLoadTimeValue = new LoadTimeValue(uniqueLTVId, analyzedEvalForm);
				currentLoadTimeValues.add(newLoadTimeValue);

				return new ImmutableLoadTimeValueStruct(uniqueLTVId);
			} else {
				return new MutableLoadTimeValueStruct(analyzedEvalForm);
			}
		} finally {
			analysisBuilder.setClosureDepth(tempClosureDepth);
			analysisBuilder.setBindingsPosition(tempBindingsPosition);
			environmentStack.pop();
		}
	}
}
