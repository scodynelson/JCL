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
import jcl.symbols.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
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
		SpecialOperatorStruct.RETURN_FROM.setMacroFunctionExpander(this);
	}

	@Override
	public ReturnFromStruct expand(final ListStruct form, final Environment environment) {

		final int formSize = form.size();
		if ((formSize < 2) || (formSize > 3)) {
			throw new ProgramErrorException("RETURN-FROM: Incorrect number of arguments: " + formSize + ". Expected either 2 or 3 arguments.");
		}

		final ListStruct formRest = form.getRest();

		final LispStruct second = formRest.getFirst();
		if (!(second instanceof SymbolStruct)) {
			final String printedObject = printer.print(second);
			throw new ProgramErrorException("RETURN-FROM: Name must be a symbol. Got: " + printedObject);
		}

		final SymbolStruct<?> name = (SymbolStruct<?>) second;
		if (environment.getBlockStack().search(name) == -1) {
			final String printedObject = printer.print(second);
			throw new ProgramErrorException("RETURN-FROM: No BLOCK with name " + printedObject + " is visible.");
		}

		final LispStruct analyzedResult;
		if (formSize == 3) {
			final ListStruct formRestRest = formRest.getRest();

			final LispStruct result = formRestRest.getFirst();
			analyzedResult = formAnalyzer.analyze(result, environment);
		} else {
			analyzedResult = NullStruct.INSTANCE;
		}

		return new ReturnFromStruct(name, analyzedResult);
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(formAnalyzer)
		                            .append(printer)
		                            .toHashCode();
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == null) {
			return false;
		}
		if (obj == this) {
			return true;
		}
		if (obj.getClass() != getClass()) {
			return false;
		}
		final ReturnFromExpander rhs = (ReturnFromExpander) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(formAnalyzer, rhs.formAnalyzer)
		                          .append(printer, rhs.printer)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(formAnalyzer)
		                                                                .append(printer)
		                                                                .toString();
	}
}
