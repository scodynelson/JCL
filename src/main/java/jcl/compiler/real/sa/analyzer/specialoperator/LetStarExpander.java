package jcl.compiler.real.sa.analyzer.specialoperator;

import java.util.List;
import java.util.ListIterator;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.sa.analyzer.expander.MacroFunctionExpander;
import jcl.compiler.real.struct.specialoperator.LetStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.printer.Printer;
import jcl.symbols.SpecialOperator;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class LetStarExpander extends MacroFunctionExpander<LetStruct> {

	private static final long serialVersionUID = 6456555635583825339L;

	@Autowired
	private LetExpander letExpander;

	@Autowired
	private Printer printer;

	/**
	 * Initializes the block macro function and adds it to the special operator 'block'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperator.LET_STAR.setMacroFunctionExpander(this);
	}

	@Override
	public LetStruct expand(final ListStruct form, final Environment environment) {

		final int formSize = form.size();
		if (formSize < 2) {
			throw new ProgramErrorException("LET*: Incorrect number of arguments: " + formSize + ". Expected at least 2 arguments.");
		}

		final ListStruct formRest = form.getRest();

		final LispStruct second = formRest.getFirst();
		if (!(second instanceof ListStruct)) {
			final String printedObject = printer.print(second);
			throw new ProgramErrorException("LET*: Parameter list must be a list. Got: " + printedObject);
		}

		final ListStruct parameters = (ListStruct) second;

		final List<LispStruct> parametersAsJavaList = parameters.getAsJavaList();
		final ListIterator<LispStruct> iterator = parametersAsJavaList.listIterator(parametersAsJavaList.size());

		ListStruct body = formRest.getRest();

		while (iterator.hasPrevious()) {
			final LispStruct previousParams = iterator.previous();

			// NOTE: Make Dotted list here so the 'contents' of the body get added to the let
			body = ListStruct.buildDottedList(SpecialOperator.LET, previousParams, body);
		}

		return letExpander.expand(body, environment);
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(letExpander)
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
		final LetStarExpander rhs = (LetStarExpander) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(letExpander, rhs.letExpander)
		                          .append(printer, rhs.printer)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(letExpander)
		                                                                .append(printer)
		                                                                .toString();
	}
}
