package jcl.compiler.sa.analyzer.specialoperator;

import java.util.Iterator;

import jcl.compiler.environment.Environment;
import jcl.compiler.functions.EvalFunction;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.function.expanders.MacroFunctionExpander;
import jcl.lang.BooleanStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.SpecialOperatorStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.ProgramErrorException;
import jcl.printer.Printer;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class LoadTimeValueExpander extends MacroFunctionExpander<LispStruct> {

	@Autowired
	private FormAnalyzer formAnalyzer;

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
		final Iterator<LispStruct> iterator = form.iterator();
		iterator.next(); // LOAD-TIME-VALUE SYMBOL

		if (!iterator.hasNext()) {
			throw new ProgramErrorException("LOAD-TIME-VALUE: Incorrect number of arguments: 0. Expected between 1 and 2 arguments.");
		}
		final LispStruct loadTimeValueForm = iterator.next();

		LispStruct readOnlyP = null;
		if (iterator.hasNext()) {
			readOnlyP = iterator.next();
		}

		if (iterator.hasNext()) {
			throw new ProgramErrorException("LOAD-TIME-VALUE: Incorrect number of arguments: 3. Expected between 1 and 2 arguments.");
		}

		if ((readOnlyP != null) && !(readOnlyP instanceof BooleanStruct)) {
			final String printedObject = printer.print(readOnlyP);
			throw new ProgramErrorException("LOAD-TIME-VALUE: Read-Only-P value must be either 'T' or 'NIL'. Got: " + printedObject);
		}

		final LispStruct analyzedLoadTimeValueForm = formAnalyzer.analyze(loadTimeValueForm, Environment.NULL);
		return evalFunction.eval(analyzedLoadTimeValueForm, Environment.NULL);
	}
}
