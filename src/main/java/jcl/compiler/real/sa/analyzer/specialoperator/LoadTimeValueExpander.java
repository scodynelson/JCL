package jcl.compiler.real.sa.analyzer.specialoperator;

import java.util.UUID;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.Environments;
import jcl.compiler.real.environment.LambdaEnvironment;
import jcl.compiler.real.sa.FormAnalyzer;
import jcl.compiler.real.struct.specialoperator.ImmutableLoadTimeValueStruct;
import jcl.compiler.real.struct.specialoperator.LoadTimeValueStruct;
import jcl.compiler.real.struct.specialoperator.MutableLoadTimeValueStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.printer.Printer;
import jcl.symbols.BooleanStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.SpecialOperatorStruct;
import jcl.system.CommonLispSymbols;
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
		SpecialOperatorStruct.LOAD_TIME_VALUE.setMacroFunctionExpander(this);
	}

	@Override
	public LoadTimeValueStruct expand(final ListStruct form, final Environment environment) {

		final int formSize = form.size();
		if ((formSize < 2) || (formSize > 3)) {
			throw new ProgramErrorException("LOAD-TIME-VALUE: Incorrect number of arguments: " + formSize + ". Expected either 2 or 3 arguments.");
		}

		final ListStruct formRest = form.getRest();
		final ListStruct formRestRest = formRest.getRest();

		LispStruct third = formRestRest.getFirst();
		if (third instanceof NullStruct) {
			third = NILStruct.INSTANCE;
		} else if (!(third instanceof BooleanStruct)) {
			final String printedObject = printer.print(third);
			throw new ProgramErrorException("LOAD-TIME-VALUE: Read-Only-P value must be either 'T' or 'NIL'. Got: " + printedObject);
		}

		final BooleanStruct readOnlyP = (BooleanStruct) third;
		final boolean isReadOnly = readOnlyP.booleanValue();

		final LispStruct loadTimeValueForm = formRest.getFirst();
		final ListStruct evalForm = ListStruct.buildProperList(CommonLispSymbols.EVAL, loadTimeValueForm);

		final LispStruct analyzedEvalForm = formAnalyzer.analyze(evalForm, Environment.NULL);

		if (isReadOnly) {
			final LambdaEnvironment enclosingLambda = Environments.getEnclosingLambda(environment);

			String fieldName = UUID.randomUUID().toString();
			while (!Character.isJavaIdentifierStart(fieldName.charAt(0))) {
				// Ensure the fieldId starts with a valid Java identifier character
				fieldName = UUID.randomUUID().toString();
			}

			final String uniqueLTVId = fieldName.replace('-', '_');
			enclosingLambda.getLoadTimeValues().put(uniqueLTVId, analyzedEvalForm);

			return new ImmutableLoadTimeValueStruct(uniqueLTVId);
		} else {
			return new MutableLoadTimeValueStruct(analyzedEvalForm);
		}
	}
}
