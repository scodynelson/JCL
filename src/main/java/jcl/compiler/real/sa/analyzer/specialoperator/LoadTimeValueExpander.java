package jcl.compiler.real.sa.analyzer.specialoperator;

import java.util.UUID;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.Environments;
import jcl.compiler.real.environment.LambdaEnvironment;
import jcl.compiler.real.environment.LoadTimeValue;
import jcl.compiler.real.sa.FormAnalyzer;
import jcl.compiler.real.sa.analyzer.expander.MacroFunctionExpander;
import jcl.compiler.real.struct.specialoperator.ImmutableLoadTimeValueStruct;
import jcl.compiler.real.struct.specialoperator.LoadTimeValueStruct;
import jcl.compiler.real.struct.specialoperator.MutableLoadTimeValueStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.printer.Printer;
import jcl.symbols.BooleanStruct;
import jcl.symbols.SpecialOperator;
import jcl.system.CommonLispSymbols;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class LoadTimeValueExpander extends MacroFunctionExpander<LoadTimeValueStruct> {

	private static final long serialVersionUID = 2168018740373766746L;

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Autowired
	private Printer printer;

	/**
	 * Initializes the load-time-value macro function and adds it to the special operator 'load-time-value'.
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
			final String printedObject = printer.print(third);
			throw new ProgramErrorException("LOAD-TIME-VALUE: Read-Only-P value must be either 'T' or 'NIL'. Got: " + printedObject);
		}

		final BooleanStruct readOnlyP = (BooleanStruct) third;
		final boolean isReadOnly = readOnlyP.booleanValue();

		final LispStruct loadTimeValueForm = inputRest.getFirst();
		final ListStruct evalForm = ListStruct.buildProperList(CommonLispSymbols.EVAL, loadTimeValueForm);

		final LispStruct analyzedEvalForm = formAnalyzer.analyze(evalForm, Environment.NULL);

		if (isReadOnly) {
			final LambdaEnvironment enclosingLambda = Environments.getEnclosingLambda(environment);

			final UUID uniqueLTVId = UUID.randomUUID();
			final LoadTimeValue newLoadTimeValue = new LoadTimeValue(uniqueLTVId, analyzedEvalForm);
			enclosingLambda.addLoadTimeValue(newLoadTimeValue);

			return new ImmutableLoadTimeValueStruct(uniqueLTVId);
		} else {
			return new MutableLoadTimeValueStruct(analyzedEvalForm);
		}
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
