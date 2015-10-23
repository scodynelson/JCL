package jcl.compiler.real.sa.analyzer.specialoperator;

import java.util.List;
import java.util.ListIterator;
import java.util.Stack;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.sa.analyzer.LispFormValueValidator;
import jcl.compiler.real.struct.specialoperator.go.GoStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.lists.ListStruct;
import jcl.numbers.IntegerStruct;
import jcl.printer.Printer;
import jcl.symbols.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class GoExpander extends MacroFunctionExpander<GoStruct<?>> {

	private static final long serialVersionUID = -6523523596100793498L;

	@Autowired
	private LispFormValueValidator validator;

	@Autowired
	private Printer printer;

	@Override
	public SymbolStruct<?> getFunctionSymbol() {
		return SpecialOperatorStruct.GO;
	}

	@Override
	public GoStruct<?> expand(final ListStruct form, final Environment environment) {
		validator.validateListFormSize(form, 2, "GO");

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
}
