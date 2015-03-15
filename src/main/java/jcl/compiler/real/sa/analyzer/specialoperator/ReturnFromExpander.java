package jcl.compiler.real.sa.analyzer.specialoperator;

import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.sa.FormAnalyzer;
import jcl.compiler.real.sa.analyzer.expander.MacroFunctionExpander;
import jcl.compiler.real.struct.specialoperator.ReturnFromStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.printer.Printer;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class ReturnFromExpander extends MacroFunctionExpander<ReturnFromStruct> {

	private static final long serialVersionUID = 3328790948675693554L;

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Autowired
	private Printer printer;

	/**
	 * Initializes the return-from macro function and adds it to the special operator 'return-from'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperator.RETURN_FROM.setMacroFunctionExpander(this);
	}

	@Override
	public ReturnFromStruct expand(final ListStruct form, final Environment environment) {

		final int inputSize = form.size();
		if ((inputSize < 2) || (inputSize > 3)) {
			throw new ProgramErrorException("RETURN-FROM: Incorrect number of arguments: " + inputSize + ". Expected either 2 or 3 arguments.");
		}

		final ListStruct inputRest = form.getRest();

		final LispStruct second = inputRest.getFirst();
		if (!(second instanceof SymbolStruct)) {
			final String printedObject = printer.print(second);
			throw new ProgramErrorException("RETURN-FROM: Name must be a symbol. Got: " + printedObject);
		}

		final SymbolStruct<?> name = (SymbolStruct<?>) second;
		if (environment.getBlockStack().search(name) == -1) {
			final String printedObject = printer.print(second);
			throw new ProgramErrorException("RETURN-FROM: No BLOCK with name " + printedObject + " is visible.");
		}

		LispStruct analyzedResult = NullStruct.INSTANCE;
		if (inputSize == 3) {
			final ListStruct inputRestRest = inputRest.getRest();

			final LispStruct result = inputRestRest.getFirst();
			analyzedResult = formAnalyzer.analyze(result, environment);
		}

		return new ReturnFromStruct(name, analyzedResult);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
