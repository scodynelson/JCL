package jcl.compiler.real.sa.analyzer.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.element.specialoperator.go.GoElement;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.numbers.IntegerStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

import java.util.ListIterator;
import java.util.Set;
import java.util.Stack;

@Component
public class GoAnalyzer implements SpecialOperatorAnalyzer {

	private static final long serialVersionUID = -6523523596100793498L;

	@Override
	public GoElement<?> analyze(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder) {

		if (input.size() != 2) {
			throw new ProgramErrorException("GO: Incorrect number of arguments: " + input.size() + ". Expected 2 arguments.");
		}

		final LispStruct second = input.getRest().getFirst();

		if (!isTagbodyTag(second)) {
			throw new ProgramErrorException("GO: Tag must be of type SymbolStruct or IntegerStruct. Got: " + second);
		}

		return getGoTag(analysisBuilder, second);
	}

	private static boolean isTagbodyTag(final LispStruct element) {
		return (element instanceof SymbolStruct) || (element instanceof IntegerStruct);
	}

	private static GoElement<?> getGoTag(final AnalysisBuilder analysisBuilder, final LispStruct tagToFind) {

		final Stack<Set<GoElement<?>>> tagbodyStack = analysisBuilder.getTagbodyStack();
		final ListIterator<Set<GoElement<?>>> tagbodyListIterator = tagbodyStack.listIterator(tagbodyStack.size());

		GoElement<?> tag = null;

		out:
		while (tagbodyListIterator.hasPrevious()) {
			final Set<GoElement<?>> previousStack = tagbodyListIterator.previous();
			for (final GoElement<?> goElement : previousStack) {
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
