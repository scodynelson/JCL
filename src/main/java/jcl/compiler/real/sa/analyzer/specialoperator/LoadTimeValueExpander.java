package jcl.compiler.real.sa.analyzer.specialoperator;

import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.functions.EvalFunction;
import jcl.compiler.real.sa.FormAnalyzer;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.printer.Printer;
import jcl.symbols.BooleanStruct;
import jcl.symbols.SpecialOperatorStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class LoadTimeValueExpander extends MacroFunctionExpander<LispStruct> {

	private static final long serialVersionUID = 2168018740373766746L;

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Autowired
	private EvalFunction evalFunction;

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
	public LispStruct expand(final ListStruct form, final Environment environment) {

		final int formSize = form.size();
		if ((formSize < 2) || (formSize > 3)) {
			throw new ProgramErrorException("LOAD-TIME-VALUE: Incorrect number of arguments: " + formSize + ". Expected either 2 or 3 arguments.");
		}

		final ListStruct formRest = form.getRest();
		final ListStruct formRestRest = formRest.getRest();

		final LispStruct third = formRestRest.getFirst();
		if (!(third instanceof BooleanStruct) && !(third instanceof NullStruct)) {
			final String printedObject = printer.print(third);
			throw new ProgramErrorException("LOAD-TIME-VALUE: Read-Only-P value must be either 'T' or 'NIL'. Got: " + printedObject);
		}

		final LispStruct loadTimeValueForm = formRest.getFirst();
		final LispStruct analyzedEvalForm = formAnalyzer.analyze(loadTimeValueForm, Environment.NULL);
		return evalFunction.eval(analyzedEvalForm, Environment.NULL);
	}
}
