package jcl.compiler.real.sa.analyzer.defstruct;

import java.util.ArrayList;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.struct.specialoperator.defstruct.DefstructStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.printer.Printer;
import jcl.symbols.NILStruct;
import jcl.symbols.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class DefstructExpander extends MacroFunctionExpander<LispStruct> {

	private static final long serialVersionUID = 5336983779662053736L;

	@Autowired
	private Printer printer;

	/**
	 * Initializes the defstruct macro function and adds it to the special operator 'defstruct'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperatorStruct.DEFSTRUCT.setMacroFunctionExpander(this);
	}

	@Override
	public DefstructStruct expand(final ListStruct form, final Environment environment) {

		final int formSize = form.size();
		if (formSize < 5) {
			throw new ProgramErrorException("%DEFSTRUCT: Incorrect number of arguments: " + formSize + ". Expected at least 3 arguments.");
		}

		final ListStruct formRest = form.getRest();

		final LispStruct second = formRest.getFirst();
		if (!(second instanceof SymbolStruct)) {
			final String printedObject = printer.print(second);
			throw new ProgramErrorException("%DEFSTRUCT: Structure name must be a symbol. Got: " + printedObject);
		}
		final SymbolStruct<?> structureSymbol = (SymbolStruct) second;

		final ListStruct formRestRest = formRest.getRest();

		final LispStruct third = formRestRest.getFirst();

		SymbolStruct<?> includeStructureSymbol = null;
		if (third instanceof SymbolStruct) {
			includeStructureSymbol = (SymbolStruct) third;
		} else if (!NILStruct.INSTANCE.equals(third) && !NullStruct.INSTANCE.equals(third)) {
			final String printedObject = printer.print(third);
			throw new ProgramErrorException("%DEFSTRUCT: Include structure name must be a symbol or NIL. Got: " + printedObject);
		}

		final ListStruct formRestRestRest = formRestRest.getRest();

		final LispStruct fourth = formRestRestRest.getFirst();

		SymbolStruct<?> defaultConstructorSymbol = null;
		if (fourth instanceof SymbolStruct) {
			defaultConstructorSymbol = (SymbolStruct) fourth;
		} else if (!NILStruct.INSTANCE.equals(fourth) && !NullStruct.INSTANCE.equals(fourth)) {
			final String printedObject = printer.print(fourth);
			throw new ProgramErrorException("%DEFSTRUCT: Include structure name must be a symbol or NIL. Got: " + printedObject);
		}

		final ListStruct formRestRestRestRest = formRestRestRest.getRest();

		final LispStruct fifth = formRestRestRestRest.getFirst();

		SymbolStruct<?> printerSymbol = null;
		if (fifth instanceof SymbolStruct) {
			printerSymbol = (SymbolStruct) fifth;
		} else if (!NILStruct.INSTANCE.equals(fifth) && !NullStruct.INSTANCE.equals(fifth)) {
			final String printedObject = printer.print(fifth);
			throw new ProgramErrorException("%DEFSTRUCT: Printer name must be a symbol or NIL. Got: " + printedObject);
		}

		final ListStruct formRestRestRestRestRest = formRestRestRestRest.getRest();

		final List<SymbolStruct<?>> slots = new ArrayList<>();

		final List<LispStruct> slotArguments = formRestRestRestRestRest.getAsJavaList();
		for (final LispStruct slotArgument : slotArguments) {
			if (!(slotArgument instanceof SymbolStruct)) {
				final String printedObject = printer.print(slotArgument);
				throw new ProgramErrorException("%DEFSTRUCT: Structure slot name must be a symbol. Got: " + printedObject);
			}

			final SymbolStruct<?> slotSymbol = (SymbolStruct<?>) slotArgument;
			slots.add(slotSymbol);
		}

		return new DefstructStruct(structureSymbol, includeStructureSymbol, defaultConstructorSymbol, printerSymbol, slots);
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
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
		final DefstructExpander rhs = (DefstructExpander) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(printer, rhs.printer)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(printer)
		                                                                .toString();
	}
}
