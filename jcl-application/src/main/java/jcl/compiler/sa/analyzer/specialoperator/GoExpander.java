package jcl.compiler.sa.analyzer.specialoperator;

import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Stack;

import jcl.compiler.environment.Environment;
import jcl.compiler.struct.specialoperator.go.GoStruct;
import jcl.compiler.function.expanders.MacroFunctionExpander;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.SpecialOperatorStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.ProgramErrorException;
import jcl.lang.number.IntegerStruct;
import jcl.printer.Printer;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class GoExpander extends MacroFunctionExpander<GoStruct<?>> {

	@Autowired
	private Printer printer;

	@Override
	public SymbolStruct getFunctionSymbol() {
		return SpecialOperatorStruct.GO;
	}

	@Override
	public GoStruct<?> expand(final ListStruct form, final Environment environment) {
		final Iterator<LispStruct> iterator = form.iterator();
		iterator.next(); // GO SYMBOL

		if (!iterator.hasNext()) {
			throw new ProgramErrorException("GO: Incorrect number of arguments: 0. Expected 1 argument.");
		}
		final LispStruct first = iterator.next();

		if (iterator.hasNext()) {
			throw new ProgramErrorException("GO: Incorrect number of arguments: 2. Expected 1 argument.");
		}

		if (!isTagbodyTag(first)) {
			final String printedObject = printer.print(first);
			throw new ProgramErrorException("GO: Tag must be a symbol or an integer. Got: " + printedObject);
		}
		return getGoTag(environment, first);

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
