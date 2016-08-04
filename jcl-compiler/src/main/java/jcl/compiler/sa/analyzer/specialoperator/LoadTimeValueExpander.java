package jcl.compiler.sa.analyzer.specialoperator;

import java.util.Iterator;

import jcl.compiler.environment.Environment;
import jcl.compiler.function.expanders.MacroFunctionExpander;
import jcl.compiler.functions.EvalFunction;
import jcl.compiler.sa.FormAnalyzer;
import jcl.lang.BooleanStructImpl;
import jcl.lang.LispStruct;
import jcl.lang.internal.SpecialOperatorStructImpl;
import jcl.lang.SymbolStructImpl;
import jcl.lang.condition.exception.ProgramErrorException;
import jcl.lang.ListStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class LoadTimeValueExpander extends MacroFunctionExpander<LispStruct> {

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Autowired
	private EvalFunction evalFunction;

	@Override
	public SymbolStructImpl getFunctionSymbol() {
		return SpecialOperatorStructImpl.LOAD_TIME_VALUE;
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

		if ((readOnlyP != null) && !(readOnlyP instanceof BooleanStructImpl)) {
			throw new ProgramErrorException("LOAD-TIME-VALUE: Read-Only-P value must be either 'T' or 'NIL'. Got: " + readOnlyP);
		}

		final LispStruct analyzedLoadTimeValueForm = formAnalyzer.analyze(loadTimeValueForm, Environment.NULL);
		return evalFunction.eval(analyzedLoadTimeValueForm, Environment.NULL);
	}
}
