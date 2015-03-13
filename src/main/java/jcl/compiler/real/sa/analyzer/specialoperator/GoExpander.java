package jcl.compiler.real.sa.analyzer.specialoperator;

import java.util.ListIterator;
import java.util.Set;
import java.util.Stack;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.AnalysisBuilder;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.sa.analyzer.expander.real.MacroFunctionExpander;
import jcl.compiler.real.struct.specialoperator.go.GoStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.numbers.IntegerStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public class GoExpander extends MacroFunctionExpander<GoStruct<?>> {

	private static final long serialVersionUID = -6523523596100793498L;

	/**
	 * Initializes the block macro function and adds it to the special operator 'block'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperator.GO.setMacroFunctionExpander(this);
	}

	@Override
	public GoStruct<?> expand(final ListStruct form, final Environment environment) {

		final int inputSize = form.size();
		if (inputSize != 2) {
			throw new ProgramErrorException("GO: Incorrect number of arguments: " + inputSize + ". Expected 2 arguments.");
		}

		final ListStruct inputRest = form.getRest();

		final LispStruct second = inputRest.getFirst();

		if (!isTagbodyTag(second)) {
			throw new ProgramErrorException("GO: Tag must be of type SymbolStruct or IntegerStruct. Got: " + second);
		}

		return getGoTag(environment, second);
	}

	private static boolean isTagbodyTag(final LispStruct element) {
		return (element instanceof SymbolStruct) || (element instanceof IntegerStruct);
	}

	private static GoStruct<?> getGoTag(final Environment environment, final LispStruct tagToFind) {

		final AnalysisBuilder analysisBuilder = environment.getAnalysisBuilder();
		final Stack<Set<GoStruct<?>>> tagbodyStack = analysisBuilder.getTagbodyStack();
		final ListIterator<Set<GoStruct<?>>> tagbodyListIterator = tagbodyStack.listIterator(tagbodyStack.size());

		GoStruct<?> tag = null;

		out:
		while (tagbodyListIterator.hasPrevious()) {
			final Set<GoStruct<?>> previousStack = tagbodyListIterator.previous();
			for (final GoStruct<?> goElement : previousStack) {
				final LispStruct goElementTag = goElement.getTag();
				if (tagToFind.equals(goElementTag)) {
					tag = goElement;
					break out;
				}
			}
		}

		if (tag == null) {
			throw new ProgramErrorException("GO: No TAGBODY with Tag " + tagToFind + " is visible.");
		}

		return tag;
	}
}
