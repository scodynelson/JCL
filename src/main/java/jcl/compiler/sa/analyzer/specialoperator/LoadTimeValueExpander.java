package jcl.compiler.sa.analyzer.specialoperator;

import jcl.LispStruct;
import jcl.compiler.environment.Environment;
import jcl.compiler.functions.EvalFunction;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.sa.analyzer.LispFormValueValidator;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.printer.Printer;
import jcl.symbols.BooleanStruct;
import jcl.symbols.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class LoadTimeValueExpander extends MacroFunctionExpander<LispStruct> {

	private static final long serialVersionUID = 2168018740373766746L;

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Autowired
	private LispFormValueValidator validator;

	@Autowired
	private EvalFunction evalFunction;

	@Autowired
	private Printer printer;

	@Override
	public SymbolStruct getFunctionSymbol() {
		return SpecialOperatorStruct.LOAD_TIME_VALUE;
	}

	@Override
	public LispStruct expand(final ListStruct form, final Environment environment) {
		validator.validateListFormSize(form, 2, 3, "LOAD-TIME-VALUE");

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
