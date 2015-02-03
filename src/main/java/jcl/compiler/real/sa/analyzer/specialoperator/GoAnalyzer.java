package jcl.compiler.real.sa.analyzer.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.element.GoElement;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.numbers.NumberStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

import java.util.ListIterator;
import java.util.Set;
import java.util.Stack;

@Component
public class GoAnalyzer implements SpecialOperatorAnalyzer {

	private static final long serialVersionUID = -6523523596100793498L;

	@Override
	public GoElement analyze(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder) {

		if (input.size() != 2) {
			throw new ProgramErrorException("GO: Incorrect number of arguments: " + input.size() + ". Expected 2 arguments.");
		}

		final LispStruct second = input.getRest().getFirst();
		if (!(second instanceof SymbolStruct) && !(second instanceof NumberStruct)) {
			throw new ProgramErrorException("GO: Tag must be of type SymbolStruct or IntegerStruct. Got: " + second);
		}

		LispStruct tag = null;

		final Stack<Set<LispStruct>> tagbodyStack = analysisBuilder.getTagbodyStack();
		final ListIterator<Set<LispStruct>> tagbodyListIterator = tagbodyStack.listIterator(tagbodyStack.size());

		while (tagbodyListIterator.hasPrevious()) {
			final Set<LispStruct> previousStack = tagbodyListIterator.previous();
			if (previousStack.contains(second)) {
				tag = second;
				break;
			}
		}

		if (tag == null) {
			throw new ProgramErrorException("GO: No TAGBODY with Tag " + second + " is visible.");
		}

		return new GoElement(tag);
	}
}
