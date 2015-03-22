package jcl.compiler.real.sa.analyzer.specialoperator;

import java.util.List;
import java.util.ListIterator;
import java.util.Stack;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.sa.analyzer.expander.MacroFunctionExpander;
import jcl.compiler.real.struct.specialoperator.go.GoStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.numbers.IntegerStruct;
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
public class GoExpander extends MacroFunctionExpander<GoStruct<?>> {

	private static final long serialVersionUID = -6523523596100793498L;

	@Autowired
	private Printer printer;

	/**
	 * Initializes the go macro function and adds it to the special operator 'go'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperatorStruct.GO.setMacroFunctionExpander(this);
	}

	@Override
	public GoStruct<?> expand(final ListStruct form, final Environment environment) {

		final int formSize = form.size();
		if (formSize != 2) {
			throw new ProgramErrorException("GO: Incorrect number of arguments: " + formSize + ". Expected 2 arguments.");
		}

		final ListStruct formRest = form.getRest();

		final LispStruct second = formRest.getFirst();
		if (!isTagbodyTag(second)) {
			final String printedObject = printer.print(second);
			throw new ProgramErrorException("GO: Tag must be a symbol or an integer. Got: " + printedObject);
		}

		return getGoTag(environment, second);
	}

	private static boolean isTagbodyTag(final LispStruct element) {
		return (element instanceof SymbolStruct) || (element instanceof IntegerStruct);
	}

	private GoStruct<?> getGoTag(final Environment environment, final LispStruct tagToFind) {

		final Stack<List<GoStruct<?>>> tagbodyStack = environment.getTagbodyStack();
		final ListIterator<List<GoStruct<?>>> tagbodyListIterator = tagbodyStack.listIterator(tagbodyStack.size());

		GoStruct<?> tag = null;

		out:
		while (tagbodyListIterator.hasPrevious()) {
			final List<GoStruct<?>> previousStack = tagbodyListIterator.previous();
			for (final GoStruct<?> goStruct : previousStack) {
				final LispStruct goTag = goStruct.getTag();
				if (tagToFind.equals(goTag)) {
					tag = goStruct;
					break out;
				}
			}
		}

		if (tag == null) {
			final String printedTagToFind = printer.print(tagToFind);
			throw new ProgramErrorException("GO: No TAGBODY with Tag " + printedTagToFind + " is visible.");
		}

		return tag;
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
		final GoExpander rhs = (GoExpander) obj;
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
